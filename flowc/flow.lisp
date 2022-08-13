(in-package #:burke/flow)

(defgeneric compute-info (datum)
  (:method ((user ir:datum)) (info:default-info)))

(defmethod compute-info ((datum ir:parameter))
  ;; NOTE: For propagation to terminate, this join must meet the ascending
  ;; chain condition.
  ;; FIXME: Currently it doesn't (e.g. member types)
  (reduce #'info:join/2 (ir:uinputs datum) :key #'ir:info))

(defmethod compute-info ((dat ir:constant))
  (make-instance 'info:info :type (type:member (ir:value dat))))

(defmethod compute-info ((inst ir:lookup))
  (let* ((inps (ir:uinputs inst))
         (syminfo (ir:info (first inps)))
         (envinfo (ir:info (second inps))))
    (make-instance 'info:info
      :type (type:lookup (info:type syminfo) (info:type envinfo)))))

(defmethod compute-info ((inst ir:cons))
  (let* ((inps (ir:uinputs inst))
         (car (ir:info (first inps)))
         (cdr (ir:info (second inps))))
    (make-instance 'info:info
      :type (type:cons (info:type car) (info:type cdr)))))

(defmethod compute-info ((inst ir:car))
  (let* ((info (ir:info (first (ir:uinputs inst))))
         (type (info:type info)))
    (make-instance 'info:info :type (type:car type))))
(defmethod compute-info ((inst ir:cdr))
  (let* ((info (ir:info (first (ir:uinputs inst))))
         (type (info:type info)))
    (make-instance 'info:info :type (type:cdr type))))

(defun forward-propagate-datum (datum info)
  (ir:map-uses
   (lambda (use)
     (let ((uinfo (ir:info use)))
       (let ((m (info:meet/2 info uinfo)))
         (when (info:subinfop m uinfo)
           (setf (ir:info use) m)
           (forward-propagate (ir:user use))))))
   datum))

(defun forward-propagate (datum)
  (forward-propagate-datum datum (compute-info datum)))
