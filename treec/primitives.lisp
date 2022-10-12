(in-package #:burke/treec)

(defvar *primitives*
  (list
   #+(or)
   (list (blookup 'syms::cons) 2 'o:cons)
   #+(or)
   (list (blookup 'syms::car)  1 'o:car)
   #+(or)
   (list (blookup 'syms::cdr)  1 'o:car)))

(defun primitive (node)
  (let* ((info (info node))
         (prim (and (typep info 'info:known-operative)
                    (assoc (info:value info) *primitives*))))
    (if prim
        (values (second prim) (third prim))
        (values nil nil))))
