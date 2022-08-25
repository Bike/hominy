(in-package #:burke/treec)

(defun %compile (ptree eparam body cenvironment environment)
  (let* ((op (convert-operative ptree eparam body cenvironment))
         (cmod (make-instance 'asm:cmodule))
         (cf (translate-operative op environment cmod))
         (code (asm:link cf))
         (closed (asm:closed cf)))
    (if (zerop (length closed)) ; not a closure, so we can use the code directly
        code
        (vm:enclose code
                    (map 'simple-vector
                         (lambda (item)
                           (cond ((eq item *static-env-link-marker*) environment)
                                 ((symbolp item) (i:lookup item environment))
                                 (t (error "How did ~a get in the closure vector?" item))))
                         closed)))))

(defun compile (combiner cenv)
  (etypecase combiner
    (i:derived-operative
     (%compile (i:ptree combiner) (i:eparam combiner)
               (i:body combiner) cenv (i:env combiner)))
    (i:applicative
     (i:wrap (compile (i:unwrap combiner) cenv)))
    (i:combiner combiner)))

(defun module ()
  "Return a Burke environment with bindings for the quick compiler."
  (i:make-fixed-environment
   '(syms::compile)
   (list (i:wrap (i:make-builtin-operative
                  (lambda (env combinand)
                    (declare (ignore env))
                    (destructuring-bind (combiner cenv) combinand
                      (compile combiner cenv)))
                  'syms::compile)))))
