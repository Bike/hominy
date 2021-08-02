(in-package #:burke)

;;;; This file defines an environment and other things for use by
;;;; compiler-generated code. It includes symbols useful for such code that
;;;; aren't defined in other environments.
;;;; In the future it would probably make sense to avoid consing up operatives
;;;; etc. at all, since there's no way for user code to access them.

(defclass compiled-operative (operative)
  ((%fun :initarg :fun :reader compiled-operative-fun :type function)
   (%enclosed :initarg :enclosed :reader enclosed :type list)))

(defmethod combine ((combiner compiled-operative) combinand env)
  (multiple-value-call (compiled-operative-fun combiner)
    (values-list (enclosed combiner))
    env combinand))

(defun initialize-runtime (env)
  (labels ((simp (f) (lambda (dynamic-env combinand)
                       (apply f dynamic-env combinand)))
           (ign (f) (lambda (dynamic-env combinand)
                      (declare (cl:ignore dynamic-env))
                      (apply f combinand)))
           (op (f) (make-instance 'builtin-operative :fun f))
           (app (f) (make-instance 'applicative :underlying (op f))))
    (declare (cl:ignore #'simp #'app))
    (define (op (ign #'eval)) '$eval env)
    (define (op (ign #'combine)) '$combine env)
    (define (op (ign #'augment)) '$augment env)
    (define (op (ign #'car)) '$car env)
    (define (op (ign #'cdr)) '$cdr env))
  env)
