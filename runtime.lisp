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

(defun caugment1 (env) (%augment1 env))
(defun caugment2 (env plist object)
  (let* ((names (coerce (plist-names plist) 'vector))
         (values (make-array (length names))))
    (bind-plist-to-vector plist object values 0)
    (%augment2 env names values)))

(defun enclose (fun enclosed)
  (make-instance 'compiled-operative :fun fun :enclosed enclosed))
