(in-package #:burke)

(defun $cvau (static-env plist eparam &rest body)
  (make-instance 'compiled-operative
    :enclosed (list static-env (initialize-runtime (make-environment)))
    :fun (compile nil (ir2cl (compile-to-ir plist eparam body)))))
