(in-package #:burke)

;;;; This file defines stuff compiled code uses/depends on.
;;;; It will get more complicated once representation selection allows
;;;; non-stupid calling conventions, among other things.

(defclass compiled-operative (operative)
  ((%fun :initarg :fun :reader compiled-operative-fun :type function)
   (%enclosed :initarg :enclosed :reader enclosed)))

(defmethod combine ((combiner compiled-operative) combinand env)
  (funcall (compiled-operative-fun combiner)
           (enclosed combiner) (cons combinand env)))

(defun caugment (env plist object)
  (labels ((aux (plist object)
             (etypecase plist
               (ignore (values nil nil))
               (null
                (unless (null object) (error "too many arguments"))
                (values nil nil))
               (symbol (values (list plist) (list object)))
               (cons
                (unless (consp object) (error "not enough arguments"))
                (multiple-value-bind (left-names left-values)
                    (aux (car plist) (car object))
                  (multiple-value-bind (right-names right-values)
                      (aux (cdr plist) (cdr object))
                    (values (append left-names right-names)
                            (append left-values right-values))))))))
    (multiple-value-call #'%augment env (aux plist object))))

(defun enclose (fun enclosed)
  (make-instance 'compiled-operative :fun fun :enclosed enclosed))
