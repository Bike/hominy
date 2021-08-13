(in-package #:burke/ir)

;;;; CPS-based IR; see "compiling with continuations, continued" for intro
;;;; This is basically and obviously equivalent to SSA, where continuing is
;;;; jumping to a block, and parameters to the continuation are phi nodes; but
;;;; it should also help with reified continuations and stuff. But mostly I
;;;; want to see how it works.
;;;; Here, a "continuation" is a sequence of bindings of temporary variables,
;;;; or static single assignments if you like, ending with a "terminator". A
;;;; terminator is either:
;;;; * (continue continuation arg) meaning to run that continuation.
;;;; * (combine continuation combiner combinand dynenv) meaning to call that
;;;;   combiner with that combinand and dynenv, and then passing the result to
;;;;   the continuation.
;;;; * (eval continuation form env)
;;;; * (sequence continuation forms env) [= (combine $sequence forms env)]
;;;; * branches which I haven't elaborated on yet.
;;;; Temporary variable bindings are to either
;;;; * constants (maybe?)
;;;; * (lookup symbol env)
;;;; * (enclose FUNCTION value)
;;;; * (augment env plist object)
;;;; * (cons car cdr)
;;;; * (car cons)
;;;; * (cdr cons)
;;;; Combinations are not allowed, so any combination terminates a continuation.
;;;; This is intentional; I'm hoping it will make inlining easier, since a
;;;; continuation never needs to be split.

;;;; cons, car, cdr are primitive because they come up a ton in the basic
;;;; language semantics, and because CwCC has them as primitive and who am I to
;;;; argue with an actual theoretician? This means some special cases in type
;;;; inference and stuff but I don't think it is anything too arduous. The
;;;; primitives do not do any kind of type checking and can only be inserted
;;;; when the compiler is sure of the types. Note that for convenience of
;;;; analysis, conses produced by the CONS primitive must never be modified;
;;;; conses fed to CAR or CDR can come from anywhere though.
;;;; eval and augment and sequence are primitive for similar reasons. Another
;;;; way of thinking about it is that they should work regardless of the
;;;; lexical environment the $vau is evaluated in; ($vau x #ignore (x)) is going
;;;; to combine x even if ($let ((combine print)) ...) is wrapped around.

;;;; A continuation has one parent and zero or more children. It is distinct
;;;; from all of them. Each continuation also has an entry-guard and
;;;; exit-guard, the particular nature of which I have not yet determined. When
;;;; a (continue ...) instruction is executed, exit guards between the current
;;;; continuation and the common ancestor of the current continuation and the
;;;; destination should be executed, and entry guards between the ancestor and
;;;; the destination should be executed. This is intended to support
;;;; unwind-protect/dynamic-wind/guard-continuation, but I haven't worked out
;;;; the details yet.
;;;; There are two distinguished continuations in each function: the start and
;;;; the return. The start is where control will be passed when the function is
;;;; entered. If code continues to the return continuation, the function
;;;; returns. The start, uniquely among continuations, has the function as its
;;;; parent.
;;;; Parentage is basically just the letcont relationship - a continuation is
;;;; a parent to another if the IR could have a letcont for the child in the
;;;; parent. This implies that parents dominate children, but not the converse.
;;;; It also implies that continuations only ever refer to ancestors or to
;;;; direct children of ancestors (because letcont binds like letrec).
;;;; Childness should also allow iterating over continuations without consing
;;;; overmuch?

;;;; A function is the compiler representation of a combiner. It has a start
;;;; continuation. When the combiner is combined, (combinand . dynenv) is passed
;;;; to its start continuation.
;;;; The parameter is to the start continuation rather than the function in
;;;; hopes of supporting tail recursion.

;;;; A function closes over a value. In general this value is
;;;; (lexenv plist eparam . body), as operatives may be constructed from
;;;; variable data rather than constants in the most general case,
;;;; e.g. from (combine $vau ...). This list is intended to be convenient to
;;;; produce from $vau by (cons dynenv combinand).

;;;; In this stage of IR, pairing is heavily used and everything deals with
;;;; a fixed number of values - usually one. A later phase should track and
;;;; reduce this consing. This should be doable after flow analyses like type
;;;; inference, for which single values should keep things simple.

;;; Continuations are second-class, at least for the moment, so we have this
;;; split hierarchy.
(defclass datum ()
  (;; NIL means "no name", not "name is NIL"
   (%name :initarg :name :initform nil :reader name)
   ;; Could be a set of USEs, but for now is a sequence. Not accessible
   (%uses :initarg :uses :initform nil :accessor %uses)))

(defmethod print-object ((o datum) s)
  (print-unreadable-object (o s :type t :identity t)
    (write (name o) :stream s))
  o)

(defun %add-use (datum use) (push use (%uses datum)))
(defun %remove-use (datum use)
  (when (null (setf (%uses datum) (delete use (%uses datum))))
    ;; No more uses: delete this datum
    (%cleanup datum)))
(defun %map-uses (function datum) (mapc function (%uses datum)) (values))

(defun map-users (function datum)
  (%map-uses (lambda (use) (funcall function (user use))) datum))

(defclass value (datum) ()) ; first-class data

(defclass parameter (value) ; parameter to a continuation
  ((%continuation :initarg :continuation :reader continuation
                  :accessor %continuation :type continuation)))

;;; The return continuation is somewhat magical, and some of these slots
;;; will be unbound.
(defclass continuation (datum)
  ((%parent :initarg :parent :accessor %parent
            :reader parent :type (or continuation function))
   ;; Could be a set (of continuations) but for now a list; operate through
   ;; add-child etc everywhere
   (%children :initarg :children :initform nil :accessor %children)
   (%entry-guard)
   (%exit-guard)
   (%parameter :initarg :parameter :accessor %parameter
               :reader parameter :type parameter)
   (%start :initarg :start :initform nil :accessor %start
           :reader start :type (or null instruction))
   (%terminator :initarg :terminator :initform nil :accessor %terminator
                :reader terminator :type (or null terminator))))

(defmethod function ((object continuation))
  (let ((parent (parent object)))
    (if (typep parent 'function)
        parent
        (function parent))))

(defun builtp (continuation)
  (and (not (null (start continuation)))
       (not (null (terminator continuation)))))

(defun map-binds (function continuation)
  (let ((term (terminator continuation)))
    (loop for i = (start continuation) then (next i)
          until (eql i term)
          do (funcall function i))))
(defun map-instructions (function continuation)
  (let ((term (terminator continuation)))
    (loop for i = (start continuation) then (next i)
          do (funcall function i)
          until (eql i term))
    (values)))

(defun add-child (continuation child) (push child (%children continuation)))
(defun %remove-child (continuation child)
  (setf (%children continuation) (delete child (%children continuation))))
(defun map-children (function continuation)
  (mapc function (%children continuation))
  (values))
(defun children (continuation) (copy-list (%children continuation)))

(defun make-continuation (&optional name parameter-name)
  (let* ((param (make-instance 'parameter :name parameter-name))
         (cont (make-instance 'continuation
                 :name name :parameter param)))
    (setf (%continuation param) cont)
    cont))

;; something a function closes over. FIXME: Should it link back to function?
(defclass enclosed (value) ())

;;; functions are (second-class) data due to enclose and local-combination.
(defclass function (datum)
  ((%module :initarg :module :initform nil :accessor %module
            :reader module :type (or null module))
   (%enclosed :initarg :enclosed :accessor %enclosed
              :reader enclosed :type enclosed)
   (%start :initarg :start :accessor %start :reader start :type continuation)
   (%rcont :initarg :rcont :accessor %rcont :reader rcont :type continuation)))

;;; No add- etc since you should be adding to the parent.
(defun map-continuations (f function)
  (labels ((aux (cont)
             (funcall f cont)
             (map-children #'aux cont)))
    (aux (start function))))

(defclass module ()
  (;; A sequence (but could be a set) of FUNCTIONs
   (%functions :initarg :functions :initform nil :accessor %functions)))

(defun add-function (module function) (push function (%functions module)))
(defun remove-function (module function)
  (setf (%functions module) (delete function (%functions module))))
(defun map-functions (function module)
  (map nil function (%functions module)))

(defclass instruction ()
  (;; The continuation this instruction is a part of.
   (%continuation :initarg :continuation :initform nil :accessor %continuation
                  :reader continuation :type (or null continuation))
   ;; The immediate predecessor of this instruction, or nil if this is the
   ;; first instruction of the continuation
   (%prev :initarg :prev :initform nil :accessor %prev
          :reader prev :type (or null bind))
   ;; A sequence of USEs
   (%uinputs :initarg :uinputs :accessor %uinputs :type cl:sequence)))

(defun map-inputs (function instruction)
  (map nil (lambda (use) (funcall function (definition use)))
       (%uinputs instruction))
  (values))
(defun inputs (instruction)
  (loop for use in (%uinputs instruction) collect (definition use)))

(defmethod function ((o instruction)) (function (continuation o)))

(defclass bind (instruction value)
  ((%next :initarg :next :initform nil :accessor %next
          :reader next :type (or null instruction))))

(defclass terminator (instruction) ())

(defclass use ()
  ((%definition :initarg :definition :reader definition :type datum
                :accessor %definition)
   (%user :initarg :user :reader user :type instruction
          :accessor %user)
   ;; Dataflow analysis should be able to get information specific to this
   ;; use and not the definition. Like the basic
   ;; (if (typep x 'foo) x #|wow it's a foo!!|# x #|but here it's not|#) stuff
   (%type)
   (%dx)))

(defclass constant (value)
  ((%value :initarg :value :reader value)))
(defun constant (value) (make-instance 'constant :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; particular instructions
;;;

(defclass lookup (bind) ()) ; inputs: symbol environment
(defclass cons (bind) ()) ; inputs: car cdr
(defclass car (bind) ()) ; inputs: cons
(defclass cdr (bind) ()) ; inputs: cons
(defclass enclose (bind) ()) ; inputs: function, enclosed
(defclass augment (bind) ()) ; inputs: env, plist, combinand

;; inputs: continuation, combiner, combinand, dynenv
(defclass combination (terminator) ())
;; inputs: continuation, combiner, enclosed, combinand, dynenv
(defclass local-combination (terminator) ())
(defclass eval (terminator) ()) ; continuation, form, env
(defclass sequence (terminator) ()) ; continuation, forms, env
(defclass continue (terminator) ()) ; inputs: continuation, argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers
;;;

(macrolet ((defread (class name n)
             `(defmethod ,name ((instruction ,class))
                (definition (nth ,n (%uinputs instruction)))))
           (defreads (class &rest names)
             `(progn
                ,@(loop for name in names for i from 0
                        collect `(defread ,class ,name ,i)))))
  (defreads lookup lname env)
  (defreads cons car cdr)
  (defreads car cons)
  (defreads cdr cons)
  (defreads enclose efunction enclosed)

  (defreads combination destination combiner combinand dynamic-environment)
  (defreads local-combination
    destination combiner enclosed combinand dynamic-environment)
  (defreads continue destination argument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAKE-INSTRUCTION - like MAKE-INSTANCE but takes care of uses
;;; It is intended that the result is immediately passed to something that
;;; actually inserts the instruction into a continuation somewhere.
;;;

(defun %make-use (def)
  (let ((use (make-instance 'use :definition def)))
    (%add-use def use)
    use))

(defun make-instruction (class &rest inputs)
  (let* ((uses (mapcar #'%make-use inputs))
         (inst (make-instance class :uinputs uses)))
    (dolist (use uses) (setf (%user use) inst))
    inst))
;; take advantage of lisp optimizations on (make-instance CONSTANT ...)
(define-compiler-macro make-instruction (class &rest inputs)
  (let ((gclass (gensym "CLASS")) (ginst (gensym "INSTRUCTION"))
        (guses (loop repeat (length inputs) collect (gensym "USE"))))
    `(let* ((,gclass ,class) ; evaluate first - real pedantry hours
            ,@(loop for guse in guses for inpf in inputs
                    collect `(,guse (%make-use ,inpf)))
            (,ginst (make-instance ,gclass :uinputs (list ,@guses))))
       (setf ,@(loop for guse in guses
                     for inpf in inputs
                     append `((%user ,guse) ,ginst)))
       ,ginst)))

;;; Like MAKE-INSTRUCTION, but accepts a name for the binding.
(defun make-bind (class name &rest inputs)
  (let* ((uses (mapcar #'%make-use inputs))
         (inst (make-instance class :name name :uinputs uses)))
    (dolist (use uses) (setf (%user use) inst))
    inst))
(define-compiler-macro make-bind (class name &rest inputs)
  (let ((gclass (gensym "CLASS")) (ginst (gensym "INSTRUCTION"))
        (guses (loop repeat (length inputs) collect (gensym "USE"))))
    `(let* ((,gclass ,class)
            ,@(loop for guse in guses for inpf in inputs
                    collect `(,guse (%make-use ,inpf)))
            (,ginst (make-instance ,gclass
                      :name ,name :uinputs (list ,@guses))))
       (setf ,@(loop for guse in guses
                     append `((%user ,guse) ,ginst)))
       ,ginst)))
