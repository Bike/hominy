(in-package #:hominy)

(defvar *dis* nil)

;;; This is sort of like CL:COMPILE in that it just slurps up the environment.
;;; (I mean, CL:COMPILE doesn't do that, but what it can do is keep constants
;;;  that cannot be dumped.)

(defun %operative-ir (enclosed)
  (let* ((einf (make-instance 'hominy/info:info :type (hominy/type:member enclosed)))
         (module (make-instance 'hominy/ir:module))
         (cf (fresh-function)))
    (hominy/ir:add-function module cf)
    (optimize-function cf einf)
    (hominy/ir:verify module)
    cf))

(defun $cvau (static-env ptree eparam &rest body)
  (let* ((enclosed (list* static-env ptree eparam body))
         (ir (%operative-ir enclosed))
         (module (hominy/ir:module ir)))
    (when *dis* (format t "~&~a~%" (hominy/ir:disassemble module)))
    (hominy/ir:verify module)
    (let* ((cls (ir2cl ir))
           (f (compile nil cls)))
      (make-instance 'compiled-operative :fun f :enclosed enclosed))))

(defun compilation-module ()
  "Return a Hominy environment with bindings for the flow compiler."
  ;; called "module" instead of "environment" because a compilation environment
  ;; is something different.
  (i:make-fixed-environment
   '(hominy/interpreter/syms::$cvau)
   (list (i:make-builtin-operative
          (lambda (env combinand) (apply #'$cvau env combinand))))))

;;; debugging
(defun operative-ir (static-env ptree eparam &rest body)
  (%operative-ir (list* static-env ptree eparam body)))
