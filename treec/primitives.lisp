(in-package #:burke/treec)

(defvar *primitives*
  '((syms::cons 2 o:cons)
    (syms::car 1 o:car)
    (syms::cdr 1 o:car)
    (syms::unwrap 1 o:unwrap)))

(defun primitive (node)
  (let* ((info (info node))
         (prim (and (typep info 'info:known-operative)
                    (assoc (info:name info) *primitives*))))
    (if prim
        (values (second prim) (third prim))
        (values nil nil))))
