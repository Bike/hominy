(in-package #:burke)

;;;; This file defines stuff compiled code uses/depends on.
;;;; It will get more complicated once representation selection allows
;;;; non-stupid calling conventions, among other things.

(defclass compiled-operative (operative)
  ((%fun :initarg :fun :reader compiled-operative-fun :type function)
   (%enclosed :initarg :enclosed :reader enclosed :accessor %enclosed)))

(defmethod combine ((combiner compiled-operative) combinand env)
  (funcall (compiled-operative-fun combiner)
           (enclosed combiner) (cons combinand env)))

(defun enclose (fun enclosed)
  (make-instance 'compiled-operative :fun fun :enclosed enclosed))
