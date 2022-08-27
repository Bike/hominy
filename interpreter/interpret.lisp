(in-package #:burke/interpreter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ignore () ()))
(defun ignorep (object) (typep object 'ignore))
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
(defun inertp (object) (typep object 'inert))
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
(defstruct (cell (:constructor make-cell (value))) value)

(defclass environment () ())
(defun environmentp (object) (typep object 'environment))

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
    (if (cell-p cell)
        (cell-value cell)
        cell)))

(defun (setf lookup) (new symbol environment)
  "Set the value of an existing binding of SYMBOL in ENVIRONMENT to NEW.
If the binding is immutable, an error is signaled.
If the symbol is not already bound, an error is signaled. This function never creates new bindings."
  (let ((cell (cell symbol environment)))
    (if (cell-p cell)
        (setf (cell-value cell) new)
        (error "Cannot modify immutable binding of ~a" symbol))))

(defun binds? (symbol environment)
  "Returns (Lisp) true iff symbol is bound in environment, directly or indirectly."
  (labels ((aux (environment)
             (if (nth-value 1 (local-cell symbol environment))
                 (return-from binds? t)
                 (map-parents #'aux environment))))
    (aux environment)
    nil))

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
  (declare (ignore new))
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
(defun %augment1 (env)
  (make-instance 'fixed-environment :parents (list env)))
(defun %augment2 (env names values)
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
  (declare (ignore new))
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

(defgeneric eval (form env))
(defgeneric combine (combiner combinand env))

(defmethod eval ((form symbol) env) (lookup form env))
(defmethod eval ((form cons) env)
  (combine (eval (car form) env) (cdr form) env))
(defmethod eval ((form null) env) (declare (cl:ignore env)) form)
(defmethod eval ((form t) env) (declare (cl:ignore env)) form)

(defun evaluator (env) (lambda (form) (eval form env)))
(defun evlis (forms env) (mapcar (evaluator env) forms))

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
(defclass derived-operative (operative)
  ((%ptree :initarg :ptree :reader ptree) 
   (%eparam :initarg :eparam :reader eparam :type (or ignore symbol))
   (%env :initarg :env :reader env)
   ;; A function that, given the dynamic environment and combinand, returns a
   ;; new environment to evaluate the body in. PLIST, EPARAM, and ENV are only
   ;; there for introspection/completeness/whatever. One use is that they can
   ;; be used when deserializing to recompute an augmenter.
   (%augmenter :initarg :augmenter :accessor augmenter :type function)
   ;; A list of forms (not just one form)
   (%body :initarg :body :reader body)))
(defmethod name ((object derived-operative))
  `(syms::$vau ,(ptree object) ,(eparam object)))
(defclass applicative (combiner)
  ((%underlying :initarg :underlying :reader unwrap :type combiner)))
(defmethod name ((app applicative)) (name (unwrap app)))

(defun operativep (object) (typep object 'operative))
(defun applicativep (object) (typep object 'applicative))
(defun wrap (combiner) (make-instance 'applicative :underlying combiner))

(defmethod combine ((combiner builtin-operative) combinand env)
  (funcall (builtin-impl combiner) env combinand))
(defmethod combine ((combiner derived-operative) combinand env)
  (apply #'$sequence
         (funcall (augmenter combiner) env combinand)
         (body combiner)))
(defmethod combine ((combiner applicative) combinand env)
  (combine (unwrap combiner) (evlis combinand env) env))

(defmethod print-object ((object combiner) stream)
  (print-unreadable-object (object stream :type t)
    (let ((name (name object)))
      (when name (write name :stream stream))))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros: A useful extension Shutt would probably detest.

(defclass macro (operative)
  ((%expander :initarg :expander :reader expander :type combiner)))
(defun make-macro (expander) (make-instance 'macro :expander expander))
(defmethod name ((op macro)) (name (expander op)))

(defmethod combine ((combiner macro) combinand env)
  (eval (combine (expander combiner) combinand env) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boolean ()
    ((%value :initarg :value :reader value :type (member t nil)))))

(defun booleanp (object) (typep object 'boolean))
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

(defun boolify (object) (if object true false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Kernel defines some of these as derived, but some of these are really dang
;;; involved that way, like $sequence, or having to derive them considerably
;;; confuses other definitions, as with $let.

(defun $sequence (env &rest forms)
  (cond ((null forms) (make-instance 'inert))
        ((null (rest forms)) (eval (first forms) env))
        (t (loop for (form . rest) on forms
                 for value = (eval form env)
                 when (null rest)
                   return value))))
