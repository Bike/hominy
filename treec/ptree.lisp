(in-package #:burke/treec)

;;;; Code for generating ptree bindings. It's lengthy, so it's here.

;;; Return a list of bindings (var index cellp)*, the index of the next free local,
;;; and the amount of stack space used.
(defun gen-operative-bindings (cfunction ptree eparam env-var)
  (let* ((lin (linearize-ptree ptree))
         (eparam-index (if (typep eparam 'i:ignore) nil 0))
         (local-env-index (cond ((not env-var) nil) (eparam-index 1) (t 0)))
         (bind-start-index (cond (local-env-index (1+ local-env-index))
                                 (eparam-index (1+ eparam-index))
                                 (t 0)))
         (cellp (if env-var t nil)))
    (multiple-value-bind (pbinds nlocals pstack)
        (cond ((eql ptree i:ignore)
               ;; parameters are ignored - do exactly nothing. Saves one stack spot.
               (values nil 0 0))
              (t
               (asm:assemble cfunction 'o:arg 0) ; push combinand for destructuring
               (gen-ptree cfunction ptree bind-start-index cellp)))
      (multiple-value-bind (dynenv-binds dynenv-stack)
          ;; The dynenv is already argument 1, so we don't need much.
          (cond ((not eparam-index) (values nil 0))
                (cellp
                 (asm:assemble cfunction 'o:arg 1 'o:make-cell 'o:set eparam-index)
                 (values (list (list eparam eparam-index cellp) 1)))
                (t
                 (asm:assemble cfunction 'o:arg 1 'o:set eparam-index)
                 (values (list (list eparam eparam-index cellp)) 1)))
        (multiple-value-bind (ebinds estack)
            ;; make (and bind) the local environment if we must.
            (cond (env-var
                   (asm:assemble cfunction 'o:closure 0) ; static environment
                   (unless (typep eparam 'i:ignore)
                     (asm:assemble cfunction 'o:ref eparam-index)) ; dynamic environment, to be bound
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
          (values (append pbinds dynenv-binds ebinds)
                  (+ nlocals (if eparam-index 1 0) (if env-var 1 0))
                  (max pstack dynenv-stack estack)))))))

;;; Generate code to do argument parsing. The arguments will be stored into registers
;;; starting with NEXT-LOCAL. If cellp is true, they'll all be cloistered.
;;; Returns three values: An alist of bindings (i.e. (symbol index cellp)*), the index
;;; of the next free local, and the amount of stack space used.
;;; The combinand should be on the top of the stack.
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
    ((cons i:ignore i:ignore)
     (asm:assemble cfunction 'o:err-if-not-cons)
     (values nil next-local 1))
    ((cons i:ignore t)
     (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:cdr)
     (multiple-value-bind (locals next-local nstack)
         (gen-ptree cfunction (cdr ptree) next-local cellp)
       (values locals next-local (max 2 nstack))))
    ((cons t i:ignore)
     (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:car)
     (multiple-value-bind (locals next-local nstack)
         (gen-ptree cfunction (car ptree) next-local cellp)
       (values locals next-local (max 2 nstack))))
    (cons
     (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:dup 'o:car)
     (multiple-value-bind (car-locals next-local carnstack)
         (gen-ptree cfunction (car ptree) next-local cellp)
       (asm:assemble cfunction 'o:cdr)
       (multiple-value-bind (cdr-locals next-local cdrnstack)
           (gen-ptree cfunction (cdr ptree) next-local cellp)
         (values (append car-locals cdr-locals)
                 next-local
                 (max (+ 2 carnstack) (+ 1 cdrnstack))))))))
