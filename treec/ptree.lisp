(in-package #:burke/treec)

;;;; Code for generating ptree bindings. It's lengthy, so it's here.

;;; Return a list of bindings (var index cellp)*, the index of the next free local,
;;; and the amount of stack space used.
(defun gen-operative-bindings (cfunction ptree eparam env-var)
  (let* ((lin (linearize-ptree ptree))
         (eparam-index (if (typep eparam 'i:ignore) nil 1))
         (local-env-index (cond ((not env-var) nil) (eparam-index 2) (t 1)))
         (cellp (if env-var t nil)))
    (multiple-value-bind (pbinds nlocals pstack)
        (gen-ptree-op cfunction ptree (1+ (or local-env-index eparam-index 0)) cellp)
      (multiple-value-bind (dynenv-binds dynenv-stack)
          ;; The dynenv is already in slot 1, so we don't need much.
          (cond ((not eparam-index) (values nil 0))
                (cellp
                 (asm:assemble cfunction 'o:ref 1 'o:make-cell 'o:set 1)
                 (values (list (list eparam eparam-index cellp) 1)))
                (t (values (list (list eparam eparam-index cellp)) 0)))
          ;; don't need to generate any code here, since the dynenv is already in slot 1.
          (if eparam-index
              (values (list (list eparam eparam-index cellp)) 0)
              (values nil 0))
        (multiple-value-bind (ebinds estack)
            ;; make (and bind) the local environment if we must.
            (cond (env-var
                   (asm:assemble cfunction 'o:closure 0) ; static environment
                   (unless (typep eparam 'i:ignore)
                     (asm:assemble cfunction 'o:ref 1)) ; dynamic environment, to be bound
                   ;; Note that we do not have to do anything special with cells here-
                   ;; since the refs will either be values or cells or what-ever anyway,
                   ;; and that's exactly what we want to put in the environment.
                   (loop for var in lin
                         do (asm:assemble cfunction 'o:ref (second (assoc var pbinds))))
                   (asm:assemble cfunction 'o:make-environment
                     (asm:constant-index cfunction (if eparam-index (list* eparam lin) lin))
                     'o:set local-env-index)
                   (values (list (list env-var local-env-index nil))
                           (+ 1 ; static environment
                              (if (typep eparam 'i:ignore) 0 1) ; dynamic environment
                              (length lin))))
                  (t (values nil 0)))
          (values (append pbinds dynenv-binds ebinds) nlocals
                  (max pstack dynenv-stack estack)))))))

;;; Generate code to do argument parsing. The arguments will be stored into registers
;;; starting with NEXT-LOCAL. If cellp is true, they'll all be cloistered.
;;; Returns three values: An alist of bindings (i.e. (symbol index cellp)*), the number of
;;; registers used (which is also the number bound), and the amount of stack space used.
(defun gen-ptree-op (cfunction ptree next-local cellp)
  ;; FIXME: Could be smarter, e.g. with a ptree of (x) we could reuse local 0 to be
  ;; X. In general we should be able to use 0 for something for any cons ptree.
  (etypecase ptree
    (null
     (asm:assemble cfunction 'o:ref 0 'o:err-if-not-null)
     (values nil 1 1))
    (symbol
     (cond (cellp
            (asm:assemble cfunction 'o:ref 0 'o:make-cell 'o:set 0)
            (values (list (list ptree 0 cellp)) 1 1))
           (t (values (list (list ptree 0 cellp)) 1 0))))
    (i:ignore (values nil 0 0))
    (cons
     (asm:assemble cfunction 'o:ref 0)
     (gen-ptree cfunction ptree next-local cellp))))

;;; Like the above, but assumes the value being bound is on the stack. Also, the second
;;; value will be the index of the next free local.
;;; At the end, destructuring will be complete and the stack will have that value popped.
(defun gen-ptree (cfunction ptree next-local cellp)
  (etypecase ptree
    (null
     (asm:assemble cfunction 'o:err-if-not-null)
     (values nil next-local 1))
    (symbol
     (when cellp (asm:assemble cfunction 'o:make-cell))
     (asm:assemble cfunction 'o:set next-local)
     (values (list (list ptree next-local cellp)) (1+ next-local) 1))
    (i:ignore (asm:assemble cfunction 'o:drop) (values nil next-local 1))
    (cons
     (labels ((aux (ptree)
                (etypecase ptree
                  (null (asm:assemble cfunction 'o:err-if-not-null)
                   (values nil 0))
                  (symbol
                   (when cellp (asm:assemble cfunction 'o:make-cell))
                   (asm:assemble cfunction 'o:set next-local)
                   (values
                    (prog1 (list (list ptree next-local cellp)) (incf next-local)) 0))
                  ((cons i:ignore i:ignore)
                   (asm:assemble cfunction 'o:err-if-not-cons)
                   (values nil 0))
                  ((cons i:ignore t)
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:cdr)
                   (multiple-value-bind (locals nstack) (aux (cdr ptree))
                     (values locals (max 1 nstack))))
                  ((cons t i:ignore)
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:car)
                   (multiple-value-bind (locals nstack) (aux (car ptree))
                     (values locals (max 1 nstack))))
                  (cons
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:dup 'o:car)
                   (multiple-value-bind (carlocals carnstack) (aux (car ptree))
                     (asm:assemble cfunction 'o:cdr)
                     (multiple-value-bind (cdrlocals cdrnstack) (aux (cdr ptree))
                       (values (append carlocals cdrlocals)
                               ;; could be (max 1...) but that's redundant with 1+
                               (max (1+ carnstack) cdrnstack))))))))
       (multiple-value-bind (binds nstack) (aux ptree)
         (values binds next-local (1+ nstack)))))))
