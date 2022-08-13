(in-package #:burke)

(defvar *dis* nil)

;;; This is sort of like CL:COMPILE in that it just slurps up the environment.
;;; (I mean, CL:COMPILE doesn't do that, but what it can do is keep constants
;;;  that cannot be dumped.)

(defun %operative-ir (enclosed)
  (let* ((einf (make-instance 'burke/info:info :type (burke/type:member enclosed)))
         (module (make-instance 'burke/ir:module))
         (cf (fresh-function)))
    (burke/ir:add-function module cf)
    (optimize-function cf einf)
    (burke/ir:verify module)
    cf))

(defun $cvau (static-env plist eparam &rest body)
  (let* ((enclosed (list* static-env plist eparam body))
         (ir (%operative-ir enclosed))
         (module (burke/ir:module ir)))
    (when *dis* (format t "~&~a~%" (burke/ir:disassemble module)))
    (burke/ir:verify module)
    (let* ((cls (ir2cl ir))
           (f (compile nil cls)))
      (make-instance 'compiled-operative :fun f :enclosed enclosed))))

(defun compilation-module ()
  "Return a Burke environment with bindings for the flow compiler."
  ;; called "module" instead of "environment" because a compilation environment
  ;; is something different.
  (i:make-fixed-environment
   '(burke/interpreter/syms::$cvau)
   (list (i:make-builtin-operative
          (lambda (env combinand) (apply #'$cvau env combinand))))))

;;; debugging
(defun operative-ir (static-env plist eparam &rest body)
  (%operative-ir (list* static-env plist eparam body)))
