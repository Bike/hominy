(in-package #:hominy/treec)

;;; Return a list of symbols in the ptree, depth first car to cdr.
(defun ptree-symbols (ptree)
  (etypecase ptree
    ((or i:ignore null) nil)
    (symbol (list ptree))
    (cons (append (ptree-symbols (car ptree)) (ptree-symbols (cdr ptree))))))

;;;; Code for generating ptree bindings. It's lengthy, so it's here.

;;; Given a ptree, eparam, local environment variable, and set of variables to encell,
;;; return a list of bindings (var index cellp)*.
;;; If the local environment variable is NIL, proceed without reification.
;;; A binding will be cellp only if either the local environment is reified,
;;; or if the variable is a member of the enclosed set.
(defun op-bindings (ptree eparam env-var enclosed-set &optional (start 0))
  ;; We order things as follows: eparam, then local environment,
  ;; then the symbols in the ptree in depth first order (going car->cdr).
  ;; If the eparam and/or the local environment aren't needed they're not bound.
  (let* ((eparam-index (if (typep eparam 'i:ignore) nil start))
         (local-env-index (cond ((not env-var) nil) (eparam-index 1) (t start)))
         ;; Index to start binding ptree symbols at.
         (index (cond (local-env-index (1+ local-env-index))
                      (eparam-index (1+ eparam-index))
                      (t start))))
    (labels ((cellp (sym) (if (or env-var (member sym enclosed-set)) t nil))
             (aux (ptree)
               (etypecase ptree
                 ((or null i:ignore) nil)
                 (symbol (prog1 (list (list ptree index (cellp ptree)))
                           (incf index)))
                 (cons (append (aux (car ptree)) (aux (cdr ptree)))))))
      (let ((result (aux ptree)))
        (when local-env-index
          ;; The local environment variable is never modified or bound in an
          ;; environment itself, so it never needs a cell.
          (push (list env-var local-env-index nil) result))
        (when eparam-index
          (push (list eparam eparam-index (cellp eparam)) result))
        result))))

(defun ptree-bindings (ptree index env-var enclosed-set)
  (labels ((cellp (sym) (if (or env-var (member sym enclosed-set)) t nil))
           (aux (ptree)
             (etypecase ptree
               ((or null i:ignore) nil)
               (symbol (prog1 (list (list ptree index (cellp ptree)))
                         (incf index)))
               (cons (append (aux (car ptree)) (aux (cdr ptree)))))))
    (aux ptree)))

;;; Generate code to bind the arguments into the frame,
;;; and to reify and bind the local environment if necessary.
;;; Return the amount of stack space used.
(defun gen-operative-bindings (cfunction ptree eparam locals env-var)
  (+
   ;; ptree
   (cond ((eql ptree i:ignore)
          ;; parameters are ignored - do exactly nothing. Saves one stack spot.
          0)
         (t
          (asm:assemble cfunction 'o:arg 0) ; push combinand for destructuring
          (gen-ptree cfunction ptree locals)))
   ;; eparam
   (if (typep eparam 'i:ignore)
       0
       (destructuring-bind (eparam index cellp) (assoc eparam locals)
         (declare (ignore eparam))
         ;; The dynenv is already argument 1, so we don't need much.
         (asm:assemble cfunction 'o:arg 1)
         (when cellp (asm:assemble cfunction 'o:construct
                       (asm:constant-index cfunction class:cell)))
         (asm:assemble cfunction 'o:set index)
         1))
   ;; local environment
   (if env-var
       (destructuring-bind (dvar index cellp) (assoc env-var locals)
         (declare (ignore dvar))
         (assert (not cellp))
         (let (;; List of variables to stick in the reified environment.
               (lin (if (typep eparam 'i:ignore)
                        (ptree-symbols ptree)
                        (cons eparam (ptree-symbols ptree)))))
           (asm:assemble cfunction 'o:closure 0) ; static env
           ;; Note that we do not have to do anything special with cells here-
           ;; since the refs will either be values or cells or what-ever anyway,
           ;; and that's exactly what we want to put in the environment.
           (loop for var in lin
                 for index = (second (assoc var locals))
                 do (asm:assemble cfunction 'o:ref index))
           (asm:assemble cfunction 'o:make-environment
             (asm:constant-index cfunction lin)
             'o:set index)
           (+ 1 (length lin))))
       0)))

;;; Break up an argument, which is on the stack.
;;; Return the number of new stack slots used.
(defun gen-ptree (cfunction ptree locals)
  (etypecase ptree
    (null (asm:assemble cfunction 'o:check-class
            (asm:constant-index cfunction class:null))
     0)
    (symbol (let ((local (assoc ptree locals)))
              (assert local)
              (when (third local)
                (asm:assemble cfunction 'o:construct
                  (asm:constant-index cfunction class:cell)))
              (asm:assemble cfunction 'o:set (second local)))
     0)
    ((cons i:ignore i:ignore)
     (asm:assemble cfunction 'o:check-class
       (asm:constant-index cfunction class:cons))
     1)
    ((cons i:ignore t)
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup 'o:check-class ccons
         'o:slot-read ccons 1))
     (max 1 (gen-ptree cfunction (cdr ptree) locals)))
    ((cons t i:ignore)
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup 'o:check-class ccons
         'o:slot-read ccons 0))
     (max 1 (gen-ptree cfunction (car ptree) locals)))
    (cons
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup 'o:check-class ccons 'o:dup
         'o:slot-read ccons 0)
       (let ((carstack (gen-ptree cfunction (car ptree) locals)))
         (asm:assemble cfunction 'o:slot-read ccons 1)
         (let ((cdrstack (gen-ptree cfunction (cdr ptree) locals)))
           (max (+ 2 carstack) (+ 1 cdrstack))))))))

;;; Like the above, but set variables rather than bind them anew.
(defun set-ptree (cfunction ptree locals)
  (etypecase ptree
    (i:ignore (asm:assemble cfunction 'o:drop) 0)
    (null (asm:assemble cfunction 'o:check-class
            (asm:constant-index cfunction class:null))
     0)
    (symbol (let ((local (assoc ptree locals)))
              (assert local)
              (if (third local) ; cell
                  (asm:assemble cfunction
                    'o:ref (second local)
                    'o:slot-write (asm:constant-index cfunction class:cell) 0)
                  (asm:assemble cfunction 'o:set (second local))))
     0)
    ((cons i:ignore i:ignore)
     (asm:assemble cfunction 'o:check-class
       (asm:constant-index cfunction class:cons))
     1)
    ((cons i:ignore t)
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup
         'o:check-class ccons 'o:slot-read ccons 1))
     (max 1 (set-ptree cfunction (cdr ptree) locals)))
    ((cons t i:ignore)
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup
         'o:check-class ccons 'o:slot-read ccons 0))
     (max 1 (set-ptree cfunction (car ptree) locals)))
    (cons
     (let ((ccons (asm:constant-index cfunction class:cons)))
       (asm:assemble cfunction 'o:dup 'o:check-class ccons 'o:dup
         'o:slot-read ccons 0)
       (let ((carstack (set-ptree cfunction (car ptree) locals)))
         (asm:assemble cfunction 'o:slot-read ccons 1)
         (let ((cdrstack (set-ptree cfunction (cdr ptree) locals)))
           (max (+ 2 carstack) (+ 1 cdrstack))))))))

;;; Generate code to parse arguments to the CEP. Returns stack used.
;;; The CEP receives arg0 = dynenv, arg1 = car of combinand, arg2 = cadr, etc.
;;; Any combiner's CEP can be called depending on the call site, even if it
;;; doesn't really make sense for the combiner.
(defun gen-cep (cfunction ptree eparam locals env-var)
  (multiple-value-bind (ptmin ptmax) (compute-argcount ptree)
    ;; The first arg is always the dynenv. And due to how the VM is set up,
    ;; we'll always have at least that first arg, and we don't need to check..
    (let ((min (1+ ptmin)) (max (if ptmax (1+ ptmax) ptmax)))
      ;; Generate code to check the argcount.
      (cond ((eql min max) (asm:assemble cfunction 'o:check-arg-count-= min))
            ((> min 1) (asm:assemble cfunction 'o:check-arg-count->= min))
            ((= min 1))
            (t (error "BUG: Impossible")))
      ;; Store the dynenv if we need to.
      (let* ((destack
               (etypecase eparam
                 (symbol (let ((elocal (assoc eparam locals)))
                           (assert elocal)
                           (asm:assemble cfunction 'o:arg 0)
                           (when (third elocal) ; cell needed
                             (asm:assemble cfunction 'o:construct
                               (asm:constant-index cfunction class:cell)))
                           (asm:assemble cfunction 'o:set (second elocal))
                           1))
                 (i:ignore 0)))
             (ptstack (gen-cep-ptree cfunction ptree locals))
             (estack
               (cond (env-var
                      ;; reify
                      (asm:assemble cfunction 'o:closure 0) ; static env
                      (loop with lindex = (second (assoc env-var locals))
                            for (sym index _) in locals
                            unless (eq sym env-var)
                              do (asm:assemble cfunction 'o:ref index)
                              and collect sym into syms
                            finally (asm:assemble cfunction 'o:make-environment
                                      (asm:constant-index cfunction syms)
                                      'o:set lindex)
                                    (return (1+ (length syms)))))
                     (t 0))))
        (max destack ptstack estack)))))

;;; Returns (values min max). max may be NIL if anything is ok.
(defun compute-argcount (ptree)
  (etypecase ptree
    (null (values 0 0))
    ((or symbol i:ignore) (values 0 nil))
    (cons (multiple-value-bind (min max) (compute-argcount (cdr ptree))
            (values (1+ min) (if max (1+ max) nil))))))

;;; Generate code to parse the ptree. Return the amount of stack used.
(defun gen-cep-ptree (cfunction ptree locals &optional (index 1))
  (etypecase ptree
    ((or null i:ignore) 0) ; argcount already checked, so nothing to do
    (symbol
     (let ((local (assoc ptree locals)))
       (assert local)
       (asm:assemble cfunction 'o:listify-args index)
       (when (third local)
         (asm:assemble cfunction
           'o:construct (asm:constant-index cfunction class:cell)))
       (asm:assemble cfunction 'o:set (second local))
       1))
    ((cons i:ignore t) ; don't care, move on without using stack
     (gen-cep-ptree cfunction (cdr ptree) locals (1+ index)))
    (cons
     (asm:assemble cfunction 'o:arg index)
     (+ (gen-ptree cfunction (car ptree) locals) 1 ; for arg
        (gen-cep-ptree cfunction (cdr ptree) locals (1+ index))))))
