(in-package #:burke)

(defun bindings->namesvec (bindings)
  (coerce (loop for (plist) in bindings nconc (plist-names plist)) 'vector))

(defun fill-values (bindings vec env)
  (loop with start = 0
        for (plist form) in bindings
        do (setf start (bind-plist-to-vector plist (eval form env) vec start))))

(defun $let (env bindings &rest body)
  (let* ((names (bindings->namesvec bindings))
         (values (make-array (length names)))
         (new-env (%augment env names values)))
    (fill-values bindings values env)
    (apply #'$sequence new-env body)))

;;; Given our fixed environments, $letrec actually can't be derived.
;;; It also has slightly different behavior from Kernel with respect to forms
;;; that immediately evaluate the newly bound names. In Kernel, doing such will
;;; get you the outside binding value if there is one, or else error with an
;;; unbound variable. (This is not stated outright but is the behavior of the
;;; given derivation.) This here binds everything to #inert. I think the ideal
;;; would be to signal an error. To do that, either there needs to be a special
;;; "unbound" marker to put in temporarily, or something like symbol macros.
;;; I'm inclined towards the latter.
(defun $letrec (env bindings &rest body)
  (let* ((names (bindings->namesvec bindings))
         (values (make-array (length names) :initial-element inert))
         (new-env (%augment env names values)))
    (fill-values bindings values new-env)
    (apply #'$sequence new-env body)))

(defun exit () (throw 'abort inert))

;;; Returns a function that, given a combinand passed
;;; to an operative, returns a new augmentation of static-env with everything
;;; in the plist and eparam bound. It sort of pre "compiles" a plist.
(defun make-augmenter (static-env plist eparam)
  (etypecase eparam
    (ignore
     (multiple-value-bind (names augmenter) (plist-augmenter plist 0)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce names 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (declare (cl:ignore dynamic-env))
           (let ((vvec (make-array nnames)))
             (funcall augmenter combinand vvec)
             (%augment static-env names-vec vvec))))))
    (symbol
     (multiple-value-bind (names augmenter) (plist-augmenter plist 1)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce (list* eparam names) 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (let ((vvec (make-array nnames)))
             (setf (svref vvec 0) dynamic-env)
             (funcall augmenter combinand vvec)
             (%augment static-env names-vec vvec))))))))

(defun $vau (static-env plist eparam &rest body)
  (let ((aug (make-augmenter static-env plist eparam)))
    (make-instance 'derived-operative
      :plist plist :eparam eparam :env static-env :augmenter aug
      ;; Used to do (cons '$sequence body) here, but then $sequence becoming
      ;; rebound would be an issue, so instead the COMBINE method has been
      ;; modified to do a sequence of forms directly.
      :body body)))

(defun $define! (env name form)
  (bind-plist name (eval form env)
              (lambda (symbol val state)
                (declare (cl:ignore state))
                (define val symbol env))
              nil)
  inert)

(defun $if (env condition then else)
  (let ((c (eval condition env)))
    (cond ((not (booleanp c))
           (error "Invalid condition for ~s: ~s evaluated to ~s"
                  '$if condition c))
          ((value c) (eval then env))
          (t (eval else env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (define (op (simp #'$letrec)) '$letrec env)
    (define (app (ign #'exit)) 'exit env)
    ;; compiler
    (define (op (simp #'$cvau)) '$cvau env))
  env)
