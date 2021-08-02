(in-package #:burke)

(defun data-to-uses (data)
  (loop for datum in data
        for use = (make-instance 'use :definition datum)
        do (add-use datum use)
        collect use))

(defvar *insert-point*)

(defun insert (inst)
  ;; Hook up the USEs
  (map-inputs (lambda (use) (setf (user use) inst)) inst)
  ;; Do the insertion
  (etypecase *insert-point*
    (cblock (setf (start *insert-point*) inst
                  (prev inst) nil
                  (cblock inst) *insert-point*))
    (instruction (setf (next *insert-point*) inst
                       (prev inst) *insert-point*
                       (cblock inst) (cblock *insert-point*))))
  (setf *insert-point* inst
        (next inst) nil)
  inst)

(defmacro build ((cblock) &body body)
  `(let ((*insert-point* ,cblock)) ,@body))

;;;

(defun build-lookup (symbol environment &optional (name (gensym "")))
  (insert (make-instance 'lookup
            :name name :inputs (data-to-uses (list symbol environment)))))

(defun build-combination (combiner env rest args &optional (name (gensym "")))
  (insert (make-instance 'combination
            :name name :inputs (data-to-uses (list* combiner env rest args)))))

(defun build-ret (value)
  (insert (make-instance 'ret :inputs (data-to-uses (list value)))))
