(in-package #:burke/interpreter)

;;; Facility for defining Burke environments full of combiners implemented in Lisp.

(defmacro defenv (name (&rest parents) &body body)
  `(defparameter ,name (envspec (,@parents) ,@body)))

(defvar *defining-environment*)

(defmacro envspec ((&rest parents) &body body)
  `(let ((*defining-environment* (make-environment ,@parents)))
     ,@body
     (copy-env-immutable *defining-environment*)))

;;; Define a Lisp function that can be used as the function of a builtin Burke operative,
;;; i.e. it takes three arguments: the dynenv, the parent frame, and the combinand.
;;; You can't declare things for the eparam or fparam, but if they're IGNORE they
;;; are ignored. Dag.
(defmacro blambda (lambda-list eparam fparam &body body)
  (let ((esym (if (eq eparam 'ignore) (gensym "DYNENV") eparam))
        (fsym (if (eq fparam 'ignore) (gensym "FRAME") fparam))
        (c (gensym "COMBINAND")))
    `(lambda (,esym ,fsym ,c)
       (declare (cl:ignore ,@(when (eq eparam 'ignore) `(,esym))
                           ,@(when (eq fparam 'ignore) `(,fsym))))
       (apply (lambda (,@lambda-list) ,@body) ,c))))

(defun isymify (symbol) (intern (symbol-name symbol) "BURKE/INTERPRETER/SYMS"))

(defmacro burke-operative (name lambda-list eparam fparam &body body)
  `(make-instance 'builtin-operative
     :fun (blambda ,lambda-list ,eparam ,fparam ,@body) :name ',(isymify name)))

(defmacro defop (name lambda-list eparam fparam &body body)
  `(define (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body)
       ',(isymify name)
     *defining-environment*))

(defmacro defapp (name lambda-list eparam fparam &body body)
  `(define (wrap (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body))
       ',(isymify name)
     *defining-environment*))

;;; Convenience: Define a type predicate.
(defmacro defpred (name lisp-name)
  `(defapp ,name (object) ignore ignore (boolify (,lisp-name object))))

(defmacro defmac (name lambda-list eparam fparam &body body)
  `(define (make-macro (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body))
       ',(isymify name) *defining-environment*))
