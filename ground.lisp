(in-package #:burke)

(defun initialize-ground (env)
  (labels ((simp (f) (lambda (dynamic-env combinand)
                       (apply f dynamic-env combinand)))
           (ign (f) (lambda (dynamic-env combinand)
                      (declare (cl:ignore dynamic-env))
                      (apply f combinand)))
           (ignb (f) (lambda (dynamic-env combinand)
                       (declare (cl:ignore dynamic-env))
                       (boolify (apply f combinand))))
           (op (f) (make-instance 'builtin-operative :fun f))
           (app (f) (make-instance 'applicative :underlying (op f))))
    ;; core semantics
    (define (app (ign #'eval)) 'eval env)
    (define (app (ign #'combine)) 'combine env)
    (define (app (ign #'lookup)) 'lookup env)
    ;; ignores
    (define (app (ignb #'ignorep)) 'ignore? env)
    ;; environments
    (define (app (ign #'environmentp)) 'environment? env)
    (define (app (ign #'make-environment)) 'make-environment env)
    (define (app (ign #'make-fixed-environment)) 'make-fixed-environment env)
    (define (op (simp #'$define!)) '$define! env)
    ;; operatives
    (define (op (simp #'$vau)) '$vau env)
    (define (app (ignb #'operativep)) 'operative? env)
    ;; applicatives
    (define (app (ignb #'applicativep)) 'applicative? env)
    (define (app (ign #'wrap)) 'wrap env)
    (define (app (ign #'unwrap)) 'unwrap env)
    ;; lists
    (define (app (ign #'cons)) 'cons env)
    (define (app (ign #'kar)) 'car env)
    (define (app (ign #'kdr)) 'cdr env)
    (define (app (ignb #'consp)) 'cons? env) ; "pair?" in kernel
    (define (app (ignb #'null)) 'null? env)
    ;; symbols
    (define (app (ignb #'symbolp)) 'symbol? env)
    ;; equivalence
    (define (app (ignb #'eql)) 'eq? env)
    ;; booleans
    (define (op (simp #'$if)) '$if env)
    (define (app (ignb #'booleanp)) 'boolean? env)
    ;; control
    (define (op (simp #'$sequence)) '$sequence env)
    (define (op (simp #'$let)) '$let env)
    (define (app (ign #'exit)) 'exit env)
    ;; compiler
    (define (op (simp #'$cvau)) '$cvau env))
  env)
