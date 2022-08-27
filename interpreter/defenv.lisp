(in-package #:burke/interpreter)

;;; Facility for defining Burke environments full of combiners implemented in Lisp.

(defmacro defenv (name (&rest parents) &body body)
  `(defparameter ,name (envspec (,@parents) ,@body)))

(defvar *defining-environment*)

(defmacro envspec ((&rest parents) &body body)
  `(let ((*defining-environment* (make-environment ,@parents)))
     ,@body
     *defining-environment*))

;;; Define a Lisp function that can be used as the function of a builtin Burke operative,
;;; i.e. it takes two arguments, the dynamic environment and the combinand.
(defmacro blambda (lambda-list eparam &body body)
  (let ((de (gensym "DYNENV")) (c (gensym "COMBINAND")))
    `(lambda (,de ,c)
       (apply
        ,(if (eq eparam 'ignore)
             (let ((dynenv (gensym "DYNENV")))
               `(lambda (,dynenv ,@lambda-list)
                  (declare (cl:ignore ,dynenv))
                  ,@body))
             `(lambda (,eparam ,@lambda-list) ,@body))
        ,de ,c))))

(defun isymify (symbol) (intern (symbol-name symbol) "BURKE/INTERPRETER/SYMS"))

(defmacro burke-operative (name lambda-list eparam &body body)
  `(make-instance 'builtin-operative
     :fun (blambda ,lambda-list ,eparam ,@body) :name ',(isymify name)))

(defmacro defop (name lambda-list eparam &body body)
  `(define (burke-operative ,name ,lambda-list ,eparam ,@body) ',(isymify name)
     *defining-environment*))

(defmacro defapp (name lambda-list eparam &body body)
  `(define (wrap (burke-operative ,name ,lambda-list ,eparam ,@body)) ',(isymify name)
     *defining-environment*))

(defmacro defmac (name lambda-list eparam &body body)
  `(define (make-macro (burke-operative ,name ,lambda-list ,eparam ,@body))
       ',(isymify name) *defining-environment*))
