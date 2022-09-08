(in-package #:burke/treec)

(defun %compile (ptree eparam body cenvironment environment)
  (let* ((static-env-var (gensym "EXTERNAL-ENVIRONMENT"))
         (op (convert-operative ptree eparam body static-env-var cenvironment))
         (cmod (make-instance 'asm:cmodule))
         (cf (translate-operative op environment cmod))
         (code (asm:link cf))
         (closed (asm:closed cf)))
    (if (zerop (length closed)) ; not a closure, so we can use the code directly
        code
        (vm:enclose code
                    (map 'simple-vector
                         (lambda (item)
                           (cond ((eq item static-env-var) environment)
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
                  (lambda (env frame combinand)
                    (declare (ignore env frame))
                    ;; FIXME: Once the compiler starts doing Burke evaluations
                    ;; (e.g. for macroexpansions), some kind of proper frame
                    ;; will need to be established.
                    ;; It is worth noting that Racket uses a continuation guard
                    ;; here, so it doesn't have to worry about keeping internal
                    ;; compiler state around for jumping back into later.
                    (destructuring-bind (combiner cenv) combinand
                      (compile combiner cenv)))
                  'syms::compile)))))
