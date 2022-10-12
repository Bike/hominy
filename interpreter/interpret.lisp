(in-package #:burke/interpreter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ignore () ()))
(defconstant ignore
  (if (boundp 'ignore) (symbol-value 'ignore) (make-instance 'ignore)))
(defmethod make-load-form ((object ignore) &optional env)
  (make-load-form-saving-slots object :environment env))
(defmethod print-object ((object ignore) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "#ignore" stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass inert () ()))
(defconstant inert
  (if (boundp 'inert) (symbol-value 'inert) (make-instance 'inert)))
(defmethod make-load-form ((object inert) &optional env)
  (make-load-form-saving-slots object :environment env))
(defmethod print-object ((object inert) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "#inert" stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Frames
;;;
;;; Frames represent control stack frames. They are not continuations, but
;;; continuations are made out of them. Every combination receives the parent
;;; frame as an argument (representing the stack frame having a link to the
;;; previous stack pointer/return address/etc). The continuation-related
;;; primitives can then make continuations out of it. This means frames can have
;;; dynamic extent and then the primitives can copy them. (the delimited
;;; continuation primitives might have to copy them, since frames are mutable.)
;;;
;;; Frames are essentially a link to the parent plus whatever information is
;;; needed to return to a point in the continuation. The root continuation
;;; has a parent of NIL to indicate it as such. Frames also have marks, which
;;; can be used to implement dynamic binding and guards, but I haven't done
;;; that yet.
;;;
;;; Interpreter frames kind of suck - making a delimited continuation out of
;;; them is going to be awkward. There needs to be enough information in each
;;; frame to reconstruct a delimited continuation later, so that's a lot of
;;; custom handling for anything we do in CL.
;;;
;;; Continuations are like Racket's, i.e. you have prompts, which are kind of
;;; like CL:CATCH, and you can capture the continuation up to a given prompt to
;;; get a delimited continuation. Originally I thought you'd just have explicit
;;; escape continuations (like CL:BLOCK): this is more primitive in a sense, but
;;; has the disadvantage that you're introducing objects that are only valid
;;; within a given dynamic extent, which is pretty much unique to them, and
;;; also makes them impossible to sensibly serialize.
;;;
;;; Unlike Racket, each prompt has two kinds of tag, the catch tag and the throw
;;; tag. Separating these is analogous to the static key thing - you have a
;;; binder and a reader, and you can choose which to make accessible to whom.
;;;
;;; Not having call/cc is pretty different from Kernel. I haven't yet decided
;;; whether to have Kernel's whole continuation hierarchy business, but I do
;;; like the general explicit design of the guards, and I like that the
;;; primitive procedures are based on operating on a continuation object rather
;;; than the current continuation implicitly (as e.g. dynamic-wind does).

;;; Implementation-wise, rather than the usual Scheme interpreter that relies
;;; on the host being tail-recursive and always invokes the continuation
;;; rather than returning normally, we represent Burke control flow as CL
;;; control flow, i.e. we just return normally and stuff. When we want to
;;; abort, we cl:throw. When we want to extend, which CL of course cannot do,
;;; we use a generic function CONTINUE, which performs all the actions in the
;;; accumulated frames. We also mark in the CL dynamic environment where the
;;; extension ends, at which point continue stops tail calling itself and just
;;; returns normally (this cannot be indicated in the frames themselves, which
;;; need to reflect the Burke frames).

;;; Frames allocated during normal evaluation are on the stack. When the code
;;; constructs a delimited continuation, the stack frames are copied into the
;;; heap. It is expected this will be comparatively rare. A lower level
;;; implementation may make a different tradeoff here. But the frames
;;; probably need to be copied anyway since they're mutable (e.g. in the VM).

;;; abstract
(defstruct frame
  (parent (error "missing arg") :type (or null frame) :read-only t))

;;; Return VALUE to FRAME and resume computation.
(defgeneric continue (frame value))

(defvar *extension-base* nil)

(defmethod continue :around ((frame frame) value)
  (if (eq frame *extension-base*)
      value
      (call-next-method)))

(defmethod continue ((frame null) value)
  (error "Somehow reached the root frame with value ~a" value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environments
;;;
;;; Mutation semantics are as follows: there are three tiers of environments:
;;; regular, fixed, and immutable. Regular environments are as described in Kernel.
;;; Fixed environments cannot have new bindings added to them. Immutable environments
;;; cannot have new bindings added and cannot have existing bindings modified.
;;; ($set! ptree value) modifies existing bindings in env. These bindings are
;;; inherited, i.e. you can modify existing bindings in a parent environment, which
;;; is pretty different from Kernel. If a binding does not already exist an error
;;; is signaled, and if the binding is in an immutable environment that's also an
;;; error. This means that unlike Kernel, you can do usual Lisp closure state stuff.
;;; ($define! ptree value) adds a binding to the dynamic environment, or modifies
;;; an existing local binding if it exists. It is not possible to add bindings to
;;; parent environments without direct access to them, providing access control
;;; similar to Kernel's.
;;;
;;; $vau (and by extension $lambda, $let, etc) create fixed environments, in
;;; order to make compilation sane. So it's not possible to e.g. shadow a
;;; name from the static environment unless you do so in the initial binding.
;;; Fixed environments can also be created directly with make-fixed-environment.
;;; Regular environments are made by make-environment and that's about it.
;;; To make an immutable environment, you can either use make-immutable-environment
;;; which has the same syntax as make-fixed-environment, or you can use the
;;; copy-env-immutable applicative, which is primarily useful for immutabilizing
;;; regular environments after stuffing a bunch of definitions into them.
;;;
;;; Hypothetically we could have environments that have immutable bindings but
;;; which you could add bindings to, but that seems pretty weird. And if you really
;;; want you can get the same effect by making a regular child of an immutable.

;;; Used for mutable bindings.
;;; These are not Burke objects, so it is not possible for an immutable environment
;;; to hold cells.
(defstruct (cell (:constructor make-cell (value))
                 (:predicate cellp))
  value)

(defclass environment () ())

(declaim (ftype (function (environment) (or environment null)) parent))
(defgeneric map-parents (function environment)
  (:argument-precedence-order environment function))
(defgeneric local-cell (symbol environment)
  (:argument-precedence-order environment symbol)
  (:documentation "Check this environment (and not parents) for a value.
If it exists, returns CELL T; if the binding is immutable returns VALUE T;
otherwise NIL NIL."))
(defgeneric define (new symbol environment)
  (:argument-precedence-order environment symbol new)
  (:documentation "Add a binding to the environment. If the symbol is already locally bound, the local binding is modified. An error is signaled if bindings cannot be added to the environment."))
(defgeneric map-bindings (function environment)
  (:argument-precedence-order environment function)
  (:documentation "Call FUNCTION on all symbols and cells in the environment (and not parents)."))

(defun cell (symbol environment)
  "Return the binding of SYMBOL in ENVIRONMENT. This is either a cell, for mutable environments, or the value, for immutable environments. If the symbol is not bound, an error is signaled. The search is carried out in Kernel's standard depth-first search order."
  (labels ((aux (environment)
             (multiple-value-bind (value presentp)
                 (local-cell symbol environment)
               (if presentp
                   (return-from cell value)
                   (map-parents #'aux environment)))))
    (aux environment)
    (error "Unbound variable ~a" symbol)))

(defun lookup (symbol environment)
  "Find the value bound to SYMBOL in ENVIRONMENT.
An environment with multiple parents is searched in depth-first order, as specified by Kernel.
Signals an error if the symbol is not bound in the environment."
  (let ((cell (cell symbol environment)))
    (if (cellp cell)
        (cell-value cell)
        cell)))

(defun (setf lookup) (new symbol environment)
  "Set the value of an existing binding of SYMBOL in ENVIRONMENT to NEW.
If the binding is immutable, an error is signaled.
If the symbol is not already bound, an error is signaled. This function never creates new bindings."
  (let ((cell (cell symbol environment)))
    (if (cellp cell)
        (setf (cell-value cell) new)
        (error "Cannot modify immutable binding of ~a" symbol))))

;;; An environment that allows mutability, suitable for global environments.
;;; This can be used like standard Kernel's environments.
;;; Implemented as a hash table of cell objects.
(defclass regular-environment (environment)
  ((%parents :initarg :parents :reader parents)
   (%table :initform (make-hash-table :test #'eq)
           :reader regular-environment-table)))

(defmethod print-object ((object regular-environment) stream)
  (print-unreadable-object (object stream :type t)
    (write (list (hash-table-count (regular-environment-table object))) :stream stream))
  object)

(defmethod map-parents (function (env regular-environment))
  (mapc function (parents env)))
(defmethod local-cell (symbol (env regular-environment))
  (multiple-value-bind (cell presentp) (gethash symbol (regular-environment-table env))
    (if presentp (values cell t) (values nil nil))))
(defmethod define (new symbol (env regular-environment))
  (let ((table (regular-environment-table env)))
    (multiple-value-bind (cell presentp) (gethash symbol table)
      (if presentp
          (setf (cell-value cell) new)
          (setf (gethash symbol table) (make-cell new)))))
  new)
(defmethod map-bindings (function (env regular-environment))
  (maphash function (regular-environment-table env)))

(defun make-environment (&rest parents)
  (make-instance 'regular-environment :parents (copy-list parents)))

;;; An environment with a fixed set of bindings, suitable for LET and etc.
;;; TODO: Large fixed (or immutable) environments could probably be accessed
;;; more efficiently through perfect hashing or something.
;;; Or at least we should do binary search.
(defclass fixed-environment (environment)
  (;; Obviously LET doesn't produce an environment with multiple parents, but
   ;; it shouldn't be especially hard to support someone doing that if they
   ;; really want to for whatever reason.
   (%parents :initarg :parents :reader parents)
   (%names :initarg :names :reader names :type (simple-array symbol (*))
           ;; Set once in %augment2, below.
           :accessor %names)
   (%vvec :initarg :vvec :reader vvec :type (simple-array cell (*))
          :accessor %vvec)))

(defmethod map-parents (function (env fixed-environment))
  (mapc function (parents env)))
(defmethod local-cell (symbol (env fixed-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))
(defmethod define (new symbol (env fixed-environment))
  (declare (cl:ignore new))
  (error "New binding for ~a cannot be added to environment ~a" symbol env))
(defmethod map-bindings (function (env fixed-environment))
  (map nil function (names env) (vvec env)))

(defun make-fixed-environment (symbols values &rest parents)
  (make-instance 'fixed-environment
    :parents (copy-list parents)
    ;; Copy the arguments, so that they can't be weirdly mutated elsewhere later.
    :names (map 'vector #'identity symbols)
    :vvec (map 'vector #'make-cell values)))

;;; Used by the compiler(s).
(defun make-fixed-environment-with-cells (symbols cells &rest parents)
  (make-instance 'fixed-environment
    :parents (copy-list parents)
    :names (map 'vector #'identity symbols)
    :vvec (map 'vector #'identity cells)))

;;; Two stage augment, used in the runtime.
(defun make-uninitialized-fixed-environment (env)
  (make-instance 'fixed-environment :parents (list env)))
(defun initialize-fixed-environment (env names values)
  (setf (%names env) (map 'vector #'identity names)
        (%vvec env) (map 'vector #'make-cell values))
  env)

(defclass immutable-environment (environment)
  ((%parents :initarg :parents :reader parents)
   (%names :initarg :names :reader names :type (simple-array symbol (*)))
   (%vvec :initarg :vvec :reader vvec :type simple-vector)))

(defmethod map-parents (function (env immutable-environment))
  (mapc function (parents env)))
(defmethod local-cell (symbol (env immutable-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))
(defmethod define (new symbol (env immutable-environment))
  (declare (cl:ignore new))
  (error "New binding for ~a cannot be added to environment ~a" symbol env))
(defmethod map-bindings (function (env immutable-environment))
  (map nil function (names env) (vvec env)))

(defun make-immutable-environment (symbols values &rest parents)
  (make-instance 'immutable-environment
    :parents (copy-list parents)
    :names (map 'vector #'identity symbols)
    :vvec (map 'vector #'identity values)))

(defun copy-env-immutable (env)
  (if (typep env 'immutable-environment)
      env
      (let ((names nil) (values nil))
        (map-bindings (lambda (name cell)
                        (push name names)
                        (push (cell-value cell) values))
                      env)
        (make-instance 'immutable-environment
          :parents (parents env)
          :names (map 'vector #'identity names)
          :vvec (map 'vector #'identity values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; In the following, the FRAME arguments are for the parent frame.
;;; This means evaluations in a tail context can be carried out by recursively
;;; invoking EVAL or whatever with the same frame as EVAL or whatever received.

(defgeneric eval (form env &optional frame))
(defgeneric combine (combiner combinand env &optional frame))
;;; This basically exists for the sake of the VM, which can be smarter.
(defgeneric call (combiner frame env &rest combinand)
  (:method (combiner env frame &rest combinand)
    (combine combiner combinand env frame)))

(defmethod eval ((form symbol) env &optional frame)
  (declare (cl:ignore frame))
  (lookup form env))
;; Representation of the interpreter evaluating a combiner form for combination
;; with some combinand in some end.
(declaim (inline make-combination-frame)) ; hopefully means SBCL can DX
(defstruct (combination-frame (:include frame)
                              (:constructor make-combination-frame
                                  (parent combinand env)))
  combinand env)
(defmethod continue ((frame combination-frame) value)
  (continue (frame-parent frame)
            (combine value (combination-frame-combinand frame)
                     (combination-frame-env frame)
                     (frame-parent frame))))
(defmethod eval ((form cons) env &optional frame)
  (let ((combinand (cdr form)))
    (combine (let ((frame (make-combination-frame frame combinand env)))
               (declare (dynamic-extent frame))
               (eval (car form) env frame))
             ;; use the original frame here - tail context
             combinand env frame)))
(defmethod eval ((form null) env &optional frame)
  (declare (cl:ignore env frame))
  form)
(defmethod eval ((form t) env &optional frame)
  (declare (cl:ignore env frame))
  form)

;; Representation of the interpreter partway through consing up the arguments
;; to an applicative.
(declaim (inline make-evlis-frame))
(defstruct (evlis-frame (:include frame)
                        (:constructor make-evlis-frame
                            (parent env so-far to-go)))
  env
  ;; The reversed list of values previously evaluated and collected.
  ;; Reversed so we don't need to append onto the end or any such crap.
  so-far
  ;; The (non-reversed) list of forms to be evaluated after this frame returns.
  to-go)

(defun evlis (forms env frame &optional vals)
  (loop for (form . remaining-forms) on forms
        do (let ((frame (make-evlis-frame frame env vals remaining-forms)))
             (declare (dynamic-extent frame))
             (push (eval form env frame) vals))
        finally (return (nreverse vals))))

(defmethod continue ((frame evlis-frame) value)
  (continue (frame-parent frame)
            (evlis (evlis-frame-to-go frame) (evlis-frame-env frame)
                   (frame-parent frame) (evlis-frame-so-far frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric name (object)) ; a nice name for printing etc.

(defclass combiner () ())
(defmethod name ((object combiner)) nil) ; give up
(defclass operative (combiner) ())
(defclass builtin-operative (operative)
  ((%fun :initarg :fun :reader builtin-impl :type function)
   (%name :initarg :name :reader name)))
(defun make-builtin-operative (function &optional name)
  "Make an operative implemented as a CL function.
The function will receive two arguments, the dynamic environment and the combinand."
  (make-instance 'builtin-operative :fun function :name name))

(defclass applicative (combiner)
  ((%underlying :initarg :underlying :reader unwrap :type combiner)))
(defmethod name ((app applicative)) (name (unwrap app)))

(defun wrap (combiner) (make-instance 'applicative :underlying combiner))

;; Operative bodies are evaluated in a tail context, so we don't need new frames
;; for this stuff.
(defmethod combine ((combiner builtin-operative) combinand env &optional frame)
  (funcall (builtin-impl combiner) env frame combinand))
;; A frame for an application (i.e. combination w/an applicative).
;; Specifically the frame is for when the arguments have not yet been eval'd,
;; which is how it's different from a combination-frame.
(declaim (inline make-app-frame))
(defstruct (app-frame (:include frame)
                      (:constructor make-app-frame
                          (parent combiner env)))
  combiner env)
(defmethod combine ((combiner applicative) combinand env &optional frame)
  (let ((ucomb (unwrap combiner)))
    (combine ucomb
             (let ((frame (make-app-frame frame ucomb env)))
               (declare (dynamic-extent frame))
               (evlis combinand env frame))
             env frame)))
(defmethod continue ((frame app-frame) value)
  (continue (frame-parent frame)
            (combine (app-frame-combiner frame) value (app-frame-env frame)
                     (frame-parent frame))))

(defmethod print-object ((object combiner) stream)
  (print-unreadable-object (object stream :type t)
    (let ((name (name object)))
      (when name (write name :stream stream))))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boolean ()
    ((%value :initarg :value :reader value :type (member t nil)))))

(defconstant true
  (if (boundp 'true) (symbol-value 'true) (make-instance 'boolean :value t)))
(defconstant false
  (if (boundp 'false)
      (symbol-value 'false)
      (make-instance 'boolean :value nil)))
(defmethod make-load-form ((object boolean) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod print-object ((object boolean) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "#~c" (if (value object) #\t #\f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User-defined classes

(defclass user-class ()
  ((%nslots :initarg :nslots :reader nslots :type (integer 0))))

;;; We could maaaaaybe implement these more efficiently by having each Burke
;;; class use a Lisp defstruct - avoid the indirect vector that way.
;;; But like who cares, man.
(defstruct (object (:constructor make-object
                       (class &aux (storage (make-array (nslots class))))))
  (class (error "missing arg") :type user-class :read-only t)
  (storage (error "missing arg") :type simple-vector :read-only t))

(defun construct-user-object (class &rest slot-values)
  (assert (= (nslots class) (length slot-values)))
  (let ((obj (make-object class)))
    (replace (object-storage obj) slot-values)
    obj))

(defun of-user-class-p (object class)
  (and (typep object 'object) (eq (object-class object) class)))

(defun slot-access (object index)
  (let ((storage (object-storage object)))
    (assert (< index (length storage)))
    (aref storage index)))

(defun (setf slot-access) (new object index)
  (let ((storage (object-storage object)))
    (assert (< index (length storage)))
    (setf (aref storage index) new)))
