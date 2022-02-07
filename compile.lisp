(in-package #:burke)

(defvar *dis* nil)

;;; This is sort of like CL:COMPILE in that it just slurps up the environment.
;;; (I mean, CL:COMPILE doesn't do that, but what it can do is keep constants
;;;  that cannot be dumped.)

(defun $cvau (static-env plist eparam &rest body)
  (let* ((enclosed (list* static-env plist eparam body))
         (einf (make-instance 'flow:info :type (type:member enclosed)))
         (module (make-instance 'ir:module))
         (cf (fresh-function)))
    (ir:add-function module cf)
    (flow:forward-propagate-datum (ir:enclosed cf) einf)
    (replace-seqs module)
    (constant-propagate module)
    (when *dis* (print (ir:disassemble module)))
    (let* ((cls (ir2cl cf))
           (f (compile nil cls)))
      (make-instance 'compiled-operative :fun f :enclosed enclosed))))
