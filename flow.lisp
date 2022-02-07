(in-package #:burke/flow)

(defgeneric compute-info (user)
  (:method ((user burke/ir:user)) (default-info)))

(defmethod compute-info ((user burke/ir:parameter))
  ;; NOTE: For propagation to terminate, this join must meet the ascending
  ;; chain condition.
  ;; FIXME: Currently it doesn't (e.g. member types)
  (reduce #'join/2 (burke/ir:uinputs user) :key #'burke/ir:info))

(defmethod compute-info ((inst burke/ir:lookup))
  (let* ((inps (burke/ir:uinputs inst))
         (syminfo (burke/ir:info (first inps)))
         (envinfo (burke/ir:info (second inps))))
    (make-instance 'info :type (type:lookup (type syminfo) (type envinfo)))))

(defmethod compute-info ((inst burke/ir:cons))
  (let* ((inps (burke/ir:uinputs inst))
         (car (burke/ir:info (first inps)))
         (cdr (burke/ir:info (second inps))))
    (make-instance 'info
      :type (type:cons (type car) (type cdr)))))

(defmethod compute-info ((inst burke/ir:car))
  (let* ((info (burke/ir:info (first (burke/ir:uinputs inst))))
         (type (type info)))
    (make-instance 'info :type (type:car type))))
(defmethod compute-info ((inst burke/ir:cdr))
  (let* ((info (burke/ir:info (first (burke/ir:uinputs inst))))
         (type (type info)))
    (make-instance 'info :type (type:cdr type))))

(defun forward-propagate-datum (datum info)
  (burke/ir:map-uses
   (lambda (use)
     (let ((uinfo (burke/ir:info use)))
       (let ((m (meet/2 info uinfo)))
         (when (subinfop m uinfo)
           (setf (burke/ir:info use) m)
           (forward-propagate-user (burke/ir:user use))))))
   datum))

(defun forward-propagate-user (user)
  (forward-propagate-datum user (compute-info user)))
