(in-package #:burke)

;;;; This file defines stuff compiled code uses/depends on.
;;;; It will get more complicated once representation selection allows
;;;; non-stupid calling conventions, among other things.

(defclass compiled-operative (i:operative)
  ((%fun :initarg :fun :reader compiled-operative-fun :type function)
   (%enclosed :initarg :enclosed :reader enclosed :accessor %enclosed)))

(defmethod i:combine ((combiner compiled-operative) combinand env)
  (funcall (compiled-operative-fun combiner)
           (enclosed combiner) (cons combinand env)))

(defun caugment1 (env) (i:make-uninitialized-fixed-environment (list env)))
(defun caugment2 (env plist object)
  (let* ((names (coerce (plist-names plist) 'vector))
         (values (make-array (length names))))
    (i:bind-plist-to-vector plist object values 0)
    (i:initialize-fixed-environment env names values)))

(defun enclose (fun enclosed)
  (make-instance 'compiled-operative :fun fun :enclosed enclosed))

(defun ccombine (combiner argument)
  (combine combiner (car argument) (cdr argument)))
