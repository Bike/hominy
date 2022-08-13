(cl:in-package #:burke)

(macrolet ((def (&rest ops)
             (let ((map (loop for op in ops for i from 0
                              collect (cons (symbol-name op) i))))
               `(progn
                  (defpackage #:burke/vm/ops
                    (:use)
                    (:export "*OPS*")
                    (:export ,@(mapcar #'symbol-name ops)))
                  (in-package #:burke/vm/ops)
                  ;; Expansion delayed until the package is actually defined
                  (dmap ,map))))
           (dmap (map)
             (let ((map (loop for (name . id) in map
                              collect (cons (find-symbol name "BURKE/VM/OPS") id))))
               `(progn
                  (defparameter ,(find-symbol "*OPS*" "BURKE/VM/OPS") ',map)
                  ,@(loop for (sym . i) in map
                          collect `(defconstant ,sym ,i))))))
  (def nop drop
    ref set closure const
    cons list car cdr
    return jump jump-if-true
    combine tail-combine lookup enclose make-environment
    call tail-call
    err-if-not-cons err-if-not-null))
