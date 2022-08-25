(in-package #:burke/treec)

;;;; Code for generating plist bindings. It's lengthy, so it's here.

;;; Return a list of bindings (var . index)*, the index of the next free local,
;;; and the amount of stack space used.
(defun gen-operative-bindings (cfunction plist eparam env-var)
  (let* ((lin (linearize-plist plist))
         (eparam-index (if (typep eparam 'i:ignore) nil 1))
         (local-env-index (cond ((not env-var) nil) (eparam-index 2) (t 1))))
    (multiple-value-bind (pbinds nlocals pstack)
        (gen-plist-op cfunction plist (1+ (or local-env-index eparam-index 0)))
      (multiple-value-bind (dynenv-binds dynenv-stack)
          ;; don't need to generate any code here, since the dynenv is already in slot 1.
          (if eparam-index
              (values (list (cons eparam eparam-index)) 0)
              (values nil 0))
        (multiple-value-bind (ebinds estack)
            ;; make (and bind) the local environment if we must.
            (cond (env-var
                   (asm:assemble cfunction 'o:closure 0) ; static environment
                   (unless (typep eparam 'i:ignore)
                     (asm:assemble cfunction 'o:ref 1)) ; dynamic environment, to be bound
                   (loop for var in lin
                         do (asm:assemble cfunction 'o:ref (cdr (assoc var pbinds))))
                   (asm:assemble cfunction 'o:make-environment
                     (asm:constant-index cfunction (if eparam-index (list* eparam lin) lin))
                     'o:set local-env-index)
                   (values (list (cons env-var local-env-index))
                           (+ 1 ; static environment
                              (if (typep eparam 'i:ignore) 0 1) ; dynamic environment
                              (length lin))))
                  (t (values nil 0)))
          (values (append pbinds dynenv-binds ebinds) nlocals
                  (max pstack dynenv-stack estack)))))))

;;; Generate code to do argument parsing. The arguments will be stored into registers
;;; starting with NEXT-LOCAL.
;;; Returns three values: An alist of bindings (i.e. (symbol . index)*), the number of
;;; registers used (which is also the number bound), and the amount of stack space used.
(defun gen-plist-op (cfunction plist next-local)
  ;; FIXME: Could be smarter, e.g. with a plist of (x) we could reuse local 0 to be
  ;; X. In general we should be able to use 0 for something for any cons plist.
  (etypecase plist
    (null
     (asm:assemble cfunction 'o:ref 0 'o:err-if-not-null)
     (values nil 1 1))
    (symbol (values (list (cons plist 0)) 1 0))
    (i:ignore (values nil 0 0))
    (cons
     (asm:assemble cfunction 'o:ref 0)
     (gen-plist cfunction plist next-local))))

;;; Like the above, but assumes the value being bound is on the stack. Also, the second
;;; value will be the index of the next free local.
;;; At the end, destructuring will be complete and the stack will have that value popped.
(defun gen-plist (cfunction plist next-local)
  (etypecase plist
    (null
     (asm:assemble cfunction 'o:err-if-not-null)
     (values nil next-local 1))
    (symbol (asm:assemble cfunction 'o:set next-local)
     (values (list (cons plist next-local)) (1+ next-local) 1))
    (i:ignore (asm:assemble cfunction 'o:drop) (values nil next-local 1))
    (cons
     (labels ((aux (plist)
                (etypecase plist
                  (null (asm:assemble cfunction 'o:err-if-not-null)
                   (values nil 0))
                  (symbol
                   (asm:assemble cfunction 'o:set next-local)
                   (values
                    (prog1 (list (cons plist next-local)) (incf next-local)) 0))
                  ((cons i:ignore i:ignore)
                   (asm:assemble cfunction 'o:err-if-not-cons)
                   (values nil 0))
                  ((cons i:ignore t)
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:cdr)
                   (multiple-value-bind (locals nstack) (aux (cdr plist))
                     (values locals (max 1 nstack))))
                  ((cons t i:ignore)
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:car)
                   (multiple-value-bind (locals nstack) (aux (car plist))
                     (values locals (max 1 nstack))))
                  (cons
                   (asm:assemble cfunction 'o:dup 'o:err-if-not-cons 'o:dup 'o:car)
                   (multiple-value-bind (carlocals carnstack) (aux (car plist))
                     (asm:assemble cfunction 'o:cdr)
                     (multiple-value-bind (cdrlocals cdrnstack) (aux (cdr plist))
                       (values (append carlocals cdrlocals)
                               ;; could be (max 1...) but that's redundant with 1+
                               (max (1+ carnstack) cdrnstack))))))))
       (multiple-value-bind (binds nstack) (aux plist)
         (values binds next-local (1+ nstack)))))))
