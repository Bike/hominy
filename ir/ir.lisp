(in-package #:burke/ir)

;;;; CPS-based IR; see "compiling with continuations, continued" for intro
;;;; This is basically and obviously equivalent to SSA, where continuing is
;;;; jumping to a block, and parameters to the continuation are phi nodes; but
;;;; it should also help with reified continuations and stuff. But mostly I
;;;; want to see how it works.
;;;; Here, a "continuation" is basically a terminator and its inputs. A
;;;; terminator is either:
;;;; * (continue continuation arg) meaning to run that continuation.
;;;; * (combine continuation combiner combinand) meaning to call that
;;;;   combiner on that combinand, and then to pass the result to the
;;;;   continuation.
;;;; * (eval continuation form env)
;;;; * (sequence continuation forms env) [= (combine $sequence forms env)]
;;;; * branches which I haven't elaborated on yet.
;;;; An input is either a constant, parameter, enclosed variable, function,
;;;; continuation, or node.
;;;; Nodes consist of computations that don't involve control flow - so either
;;;; no computation at all, or side-effect-free ones. They can still be pretty
;;;; involved, the details being left to the later code generation stages.
;;;; Node computations do not have any order, which is fine since they're
;;;; side-effect-free. They are even allowed to input to themselves or
;;;; otherwise circularly, which should be good for circular lists and
;;;; self-referencing closures.
;;;; Possible nodes are:
;;;; * (lookup symbol env)
;;;; * (enclose FUNCTION value)
;;;; * (augment env plist object)
;;;; * (cons car cdr)
;;;; * (car cons)
;;;; * (cdr cons)
;;;; Combinations are not inputs, so any combination terminates a continuation.
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

;;;; A function closes over a value. Initially this value is
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

(defgeneric %cleanup (datum)
  (:method-combination progn)
  (:method progn ((datum datum))))

(defun %add-use (datum use) (push use (%uses datum)))
(defgeneric %remove-use (datum use))
(defmethod %remove-use ((datum datum) use)
  (when (null (setf (%uses datum) (delete use (%uses datum))))
    (%cleanup datum)))
(defun map-uses (function datum) (mapc function (%uses datum)) (values))
(defun unusedp (datum) (null (%uses datum)))

(defun map-users (function datum)
  (map-uses (lambda (use) (funcall function (user use))) datum))

(defclass value (datum) ()) ; first-class data

(defclass user ()
  (;; A sequence of USEs. NOTE that PARAMETER only actually needs a set, so
   ;; maybe we should handle this more intelligently. INSTRUCTION does need an
   ;; ordered sequence.
   (%uinputs :initarg :uinputs :reader uinputs :accessor %uinputs
             :type cl:sequence)))

(defun map-inputs (function user)
  (map nil (lambda (use) (funcall function (definition use)))
       (%uinputs user))
  (values))
(defun inputs (user)
  (loop for use in (%uinputs user) collect (definition use)))

(defmethod %cleanup progn ((datum user))
  (let ((uins (%uinputs datum)))
    (setf (%uinputs datum) nil)
    (mapc #'%cleanup-use uins)))

;;; Parameter to a continuation. Its inputs are the terminators that branch to
;;; the continuation. A start continuation may have no inputs, meaning the
;;; function parameter is used as the parameter to the continuation.
;;; TODO: Represent that explicitly maybe?
(defclass parameter (value user)
  ((%continuation :initarg :continuation :reader continuation
                  :accessor %continuation :type continuation)
   (%uinputs :initform nil)))

(defun %add-uinput (use parameter)
  (push use (%uinputs parameter)))

;;; The return continuation is somewhat magical, and some of these slots
;;; will be unbound. The parent will be the function's start continuation.
;;; Note that, as a datum, a continuation represents itself, not its output.
;;; The output ends up as a parameter, if anything.
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
   (%terminator :initarg :terminator :initform nil :accessor %terminator
                :reader terminator :type (or null terminator))))

(defmethod function ((object continuation))
  (let ((parent (parent object)))
    (if (typep parent 'function)
        parent
        (function parent))))

(defun builtp (continuation)
  (and (not (null (terminator continuation)))))

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

(defun map-instructions (f function)
  (let ((seen ()))
    (map-continuations
     (lambda (cont)
       (labels ((aux (inst)
                  (unless (or (member inst seen)
                              (not (typep inst 'instruction)))
                    (push inst seen)
                    (funcall f inst)
                    (map-inputs #'aux inst))))
         (aux (terminator cont))))
     function)))

(defclass module ()
  (;; A sequence (but could be a set) of FUNCTIONs
   (%functions :initarg :functions :initform nil :accessor %functions)
   ;; A sequence (could be set) of constants in this module.
   (%constants :initarg :constants :initform () :accessor %constants
               :type cl:sequence)))

(defun add-function (module function)
  (setf (%module function) module)
  (push function (%functions module)))
(defun remove-function (module function)
  (setf (%module function) nil
        (%functions module) (delete function (%functions module))))
(defun map-functions (function module)
  (map nil function (%functions module)))

;;; Shared superclass of nodes and terminators.
(defclass instruction (value user)
  (;; The continuation this instruction is a part of.
   (%continuation :initarg :continuation :initform nil :accessor %continuation
                  :reader continuation :type (or null continuation))))

(defmethod function ((o instruction)) (function (continuation o)))

(defclass node (instruction) ())

(defclass terminator (instruction) ())

(defclass use ()
  ((%definition :initarg :definition :accessor definition :type datum
                ;; FIXME: redundant, delete
                :accessor %definition)
   (%user :initarg :user :reader user :type instruction
          :accessor %user)
   ;; Dataflow analysis should be able to get information specific to this
   ;; use and not the definition. Like the basic
   ;; (if (typep x 'foo) x #|wow it's a foo!!|# x #|but here it's not|#) stuff
   (%info :initform (flow:default-info) :type flow:info :accessor info)))

(defmethod (setf definition) :before (new-def (use use))
  (%remove-use (definition use) use))
(defmethod (setf definition) :after (new-def (use use))
  (%add-use new-def use))

(defun %cleanup-use (use)
  (let ((def (%definition use)))
    (setf (%definition use) nil)
    (%remove-use def use)))

(defclass constant (value)
  ((%value :initarg :value :reader value)))
(defun constant (value module)
  (or (find value (%constants module) :key #'value)
      (let ((c (make-instance 'constant :value value)))
        (push c (%constants module))
        c)))

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

(defun make-instruction (class name &rest inputs)
  (let* ((uses (mapcar #'%make-use inputs))
         (inst (make-instance class :name name :uinputs uses)))
    (dolist (use uses) (setf (%user use) inst))
    inst))
;; take advantage of lisp optimizations on (make-instance CONSTANT ...)
(define-compiler-macro make-instruction (class name &rest inputs)
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
