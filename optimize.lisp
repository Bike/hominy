(in-package #:burke)

(defun constant-type-p (type)
  (and (typep type 'type:member)
       (= (length (type:elements type)) 1)))

(defun constant-type-value (type)
  (first (type:elements type)))

(defun constant-propagate-function (function)
  (let ((module (ir:module function)))
    (ir:map-instructions
     (lambda (inst)
       (loop for use in (ir:uinputs inst)
             for info = (ir:info use)
             for type = (flow:type info)
             when (and (constant-type-p type)
                       (not (typep (ir:definition use) 'ir:constant)))
               do (setf (ir:definition use)
                        (ir:constant (constant-type-value type) module))))
     function)))

(defun constant-propagate (module)
  (ir:map-functions #'constant-propagate-function module))
