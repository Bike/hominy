(cl:in-package #:burke)

(macrolet ((def (&rest ops)
             (let ((map (loop for (op len) in ops for i from 0
                              collect (list (symbol-name op) i len))))
               `(progn
                  (defpackage #:burke/vm/ops
                    (:use)
                    (:export "*OPS*")
                    (:export ,@(mapcar #'car map)))
                  (in-package #:burke/vm/ops)
                  ;; Expansion delayed until the package is actually defined
                  (dmap ,map))))
           (dmap (map)
             (let ((map (loop for (name id len) in map
                              for sym = (find-symbol name "BURKE/VM/OPS")
                              collect (list sym id len))))
               `(progn
                  (defparameter ,(find-symbol "*OPS*" "BURKE/VM/OPS") ',map)
                  ,@(loop for (sym i) in map
                          collect `(defconstant ,sym ,i))))))
  (def (nop 0) (drop 0) (dup 0)
    (ref 1) (set 1) (closure 1) (const 1)
    (arg 1) (listify-args 1) (check-arg-count-= 1) (check-arg-count->= 1)
    (make-cell 0) (cell-ref 0) (cell-set 0)
    (cons 0) (list 1) (car 0) (cdr 0)
    (return 0) (jump 1) (jump-if-true 1)
    (combine 0) (tail-combine 0) (lookup 0) (unwrap 0) (wrap 0)
    (enclose 1) (make-environment 1)
    (call 1) (tail-call 1)
    (err-if-not-cons 0) (err-if-not-null 0) (err-if-not-bool 0)))
