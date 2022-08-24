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

(defclass environment () ())
(defun environmentp (object) (typep object 'environment))

(declaim (ftype (function (environment) (or environment null)) parent))
(defgeneric map-parents (function environment)
  (:argument-precedence-order environment function))
(defgeneric local-lookup (symbol environment)
  (:argument-precedence-order environment symbol)
  (:documentation "Check this environment (and not parents) for a value.
If it exists, returns VALUE T; otherwise NIL NIL."))
(defgeneric define (new symbol environment)
  (:argument-precedence-order environment symbol new)
  (:documentation "Modify an existing binding, or create a new one.
Note that it is not possible to create or modify bindings in a parent."))
(defgeneric map-bindings (function environment)
  (:argument-precedence-order environment function)
  (:documentation "Call FUNCTION on all symbols and values locally bound."))

(defun lookup (symbol environment)
  "Find the value bound to SYMBOL in ENVIRONMENT.
An environment with multiple parents is searched in depth-first order, as specified by Kernel.
Signals an error if the symbol is not bound in the environment."
  (labels ((aux (environment)
             (multiple-value-bind (value presentp)
                 (local-lookup symbol environment)
               (if presentp
                   (return-from lookup value)
                   (map-parents #'aux environment)))))
    (aux environment)
    (error "Unbound variable ~a" symbol)))

;;; An environment that allows mutability, suitable for global environments.
;;; This can be used like standard Kernel's environments.
;;; Implemented as a hash table of cell objects.
(defclass regular-environment (environment)
  ((%parents :initarg :parents :reader parents)
   (%table :initform (make-hash-table :test #'eq)
           :reader regular-environment-table)))

(defun cell (symbol regular-environment)
  (gethash symbol (regular-environment-table regular-environment)))
(defun (setf cell) (new symbol regular-environment)
  (setf (gethash symbol (regular-environment-table regular-environment)) new))

(defmethod map-parents (function (env regular-environment))
  (mapc function (parents env)))
(defmethod local-lookup (symbol (env regular-environment))
  (multiple-value-bind (cell presentp) (cell symbol env)
    (if presentp (values (car cell) t) (values nil nil))))
(defmethod define (new symbol (env regular-environment))
  (multiple-value-bind (cell presentp) (cell symbol env)
    (if presentp
        (setf (car cell) new)
        (setf (cell symbol env) (list new))))
  new)
(defmethod map-bindings (function (env regular-environment))
  ;; FIXME: This function is basically used for linking, and linking will need to
  ;; be more aware of cells in the future.
  (maphash (lambda (sym cell) (funcall function sym (car cell)))
           (regular-environment-table env)))

(defun make-environment (&rest parents)
  (make-instance 'regular-environment :parents parents))

;;; An environment with a fixed set of bindings, suitable for LET and etc.
;;; FIXME: Could also have cells. Honestly cells should probably be a totally
;;; separate thing?
(defclass fixed-environment (environment)
  (;; Obviously LET doesn't produce an environment with multiple parents, but
   ;; it shouldn't be especially hard to support someone doing that if they
   ;; really want to for whatever reason.
   (%parents :initarg :parents :reader parents)
   (%names :initarg :names :reader names :type (simple-array symbol (*))
           ;; Set once in %augment2, below.
           :accessor %names)
   (%vvec :initarg :vvec :reader vvec :type simple-vector
          :accessor %vvec)))

(defmethod map-parents (function (env fixed-environment))
  (mapc function (parents env)))
(defmethod local-lookup (symbol (env fixed-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))
(defmethod define (new symbol (env fixed-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (setf (aref (vvec env) pos) new)
        (error "New bindings cannot be added to a fixed environment ~a" env))))
(defmethod map-bindings (function (env fixed-environment))
  (map function (names env) (vvec env)))

(defun %augment (env names values)
  (make-instance 'fixed-environment
    :parents (list env)
    :names (coerce names 'vector) :vvec (coerce values 'vector)))

;;; Two stage augment, used in the runtime.
(defun %augment1 (env)
  (make-instance 'fixed-environment :parents (list env)))
(defun %augment2 (env names values)
  (setf (%names env) (coerce names 'vector)
        (%vvec env) (coerce values 'vector))
  env)

(defun make-fixed-environment (symbols values &rest parents)
  (make-instance 'fixed-environment
    :parents parents
    :names (coerce symbols 'vector)
    :vvec (coerce values 'vector)))

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
  ((%plist :initarg :plist :reader plist) 
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
  `(syms::$vau ,(plist object) ,(eparam object)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kar (cons)
  (if (consp cons)
      (car cons)
      (error 'type-error :expected-type 'cons :datum cons)))
(defun kdr (cons)
  (if (consp cons)
      (cdr cons)
      (error 'type-error :expected-type 'cons :datum cons)))
