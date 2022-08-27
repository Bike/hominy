(in-package #:burke/interpreter)

;;;; This file defines Kernel's keyed static variables.
;;;; These are like symbols, in that they can be bound. They are unlike symbols in that
;;;; 1) for each binding, there are distinct _binder_ and _reader_ objects
;;;; 2) they are not interned
;;;; These properties allow them to be used like gensyms for macro purposes. You can get "perfect
;;;; hygeine", as Shutt puts it, because you can give something access to the reader without
;;;; giving it access to binding, so there is no possibility of shadowing. And since they are not
;;;; interned, you don't need to worry about the reader suddenly creating a shadowing symbol.
;;;; They can also be garbage collected freely.

;;;; Unlike Shutt, rather than put by having the static keys be combiners, we introduce new types,
;;;; one for the binders and one for the readers. These can be used to implement Shutt's combiners
;;;; pretty straightforwardly.

;;;; Also unlike Shutt, we let these have names for the sake of human readability.

;;;; Possible extensions include mutating static bindings, and adding new static bindings to an
;;;; existing environment. I can't think of a purpose for the latter right this second, but I can
;;;; imagine the former being useful, again for hygeine reasons.

;;; Most of the below implementation would be considerably simplified by just using a weak-key
;;; hash table. But doing it this way is kind of educational. The optimum I guess might be some kind
;;; of perfect hashing?

;;; The reader. This will be the actual key in the environment, which does not refer to the binder.
(defstruct (static-key (:constructor %make-static-key (name)))
  (name nil :type symbol :read-only t))
;;; The binder. Contains a weak pointer to the reader.
(defstruct (static-binder (:constructor %make-static-binder (key-pointer)))
  (key-pointer (error "missing arg") :read-only t))

(defmethod print-object ((object static-key) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (static-key-name object) :stream stream))
  object)

(defmethod print-object ((object static-binder) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((key (trivial-garbage:weak-pointer-value (static-binder-key-pointer object))))
      (if key
          (write (static-key-name key) :stream stream)
          (write-string "[defunct]" stream))))
  object)

(defun make-static-key (&optional name)
  (let* ((key (%make-static-key name))
         (keyp (trivial-garbage:make-weak-pointer key))
         (binder (%make-static-binder keyp)))
    (values binder key)))

(defclass static-environment (environment) ())

(defmethod define (new (name static-key) (env static-environment))
  (declare (ignore new))
  (error "New bindings cannot be added to a static environment"))

(defclass static-fixed-environment (static-environment)
  ((%parents :initarg :parents :reader parents)
   ;; A vector of weak pointers to static keys.
   (%names :initarg :names :reader names :type simple-vector)
   (%vvec :initarg :vvec :reader vvec :type simple-vector)))

(defmethod map-parents (function (env static-fixed-environment))
  (mapc function (parents env)))
;;; Although static environments are environments, for normal evaluation purposes they are empty.
;;; LOOKUP does not work on static keys; you have to use static-lookup.
;;; This is to avoid any punning confusion, to keep the evaluation rules simple and unambiguous,
;;; and to compartmentalize static environments.
(defmethod local-lookup (symbol (env static-fixed-environment)) (values nil nil))
(defmethod map-bindings (function (env static-fixed-environment)) (declare (ignore function)))

(defgeneric local-static-lookup (key environment))

(defmethod local-static-lookup ((key static-key) (env environment)) (values nil nil))

(defmethod local-static-lookup ((name static-key) (env static-fixed-environment))
  (let ((pos (position name (names env) :key #'trivial-garbage:weak-pointer-value)))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))

(defun static-lookup (key environment)
  (labels ((aux (environment)
             (multiple-value-bind (value presentp) (local-static-lookup key environment)
               (if presentp
                   (return-from static-lookup value)
                   (map-parents #'aux environment)))))
    (aux environment)
    (error "Unbound static key ~a" key)))

(defun make-static-fixed-environment (binders nvalues &rest parents)
  (let ((names nil) (values nil))
    (map nil (lambda (binder value)
               (let* ((keyp (static-binder-key-pointer binder))
                      (key (trivial-garbage:weak-pointer-value keyp)))
                 ;; If a key has been garbage collected, nothing could possibly access a new binding,
                 ;; so don't even bother making it.
                 (when key
                   ;; Reusing a weak pointer is okay, right? I don't see why it wouldn't be.
                   (push keyp names)
                   (push value values))))
         binders nvalues)
    (let ((namevec (coerce names 'vector))
          (vvec (coerce values 'vector)))
      (let ((i 0))
        (map nil (lambda (name)
                   ;; When a name is GC'd, stomp the value to make it collectable.
                   (let ((key (trivial-garbage:weak-pointer-value name)))
                     (if key
                         (trivial-garbage:finalize
                          ;; the LET is to make sure we get a new closure.
                          name (let ((i i)) (lambda () (setf (svref vvec i) nil))))
                         ;; Wow, the key was collected while we're in the middle of trying to bind it.
                         ;; In that case, axe the value in vvec to prevent a leak.
                         (setf (svref vvec i) nil)))
                   (incf i))
             namevec))
      (make-instance 'static-fixed-environment
        :parents parents :names namevec :vvec vvec))))

(defenv *static* ()
  (defop  $make-static-key (&optional name) ignore
    (multiple-value-list (make-static-key name)))
  (defapp make-static-fixed-environment (binders values &rest parents) ignore
    (apply #'make-static-fixed-environment binders values parents))
  (defapp static-lookup (key environment) ignore (static-lookup key environment))
  ;; convenience applicative - like static-lookup but uses the dynenv.
  (defapp static-variable (key) dynenv (static-lookup key dynenv))
  (defapp static-key? (object) ignore (static-key-p object))
  (defapp static-binder? (object) ignore (static-binder-p object)))

#|
;;; Here is how make-keyed-static-variable would be implemented in terms of these.
($define! make-keyed-static-variable
  ($lambda ()
    ($let (((binder key) (make-static-key)))
      (list ($lambda (value env) (make-static-fixed-environment (list binder) (list value) env))
            (wrap ($vau () dynenv (static-lookup key dynenv)))))))
|#
