(in-package #:burke/baselib)

;;; Facility for defining Burke environments full of combiners implemented in Lisp.

(defmacro defenv (name (&rest parents) &body body)
  (if (symbolp name)
      `(defparameter ,name (envspec (,@parents) ,@body))
      `(progn
         (defvar ,(first name))
         (defvar ,(second name))
         (setf (values ,(first name) ,(second name))
               (envspec (,@parents) ,@body)))))

(defvar *defining-environment*)
(defvar *defining-compilation-bindings*)

(defmacro envspec ((&rest parents) &body body)
  `(let ((*defining-environment* (i:make-environment ,@parents))
         (*defining-compilation-bindings* nil))
     ,@body
     (values (i:copy-env-immutable *defining-environment*)
             (apply #'cenv:make-cenv nil t *defining-compilation-bindings*))))

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
  `(i:make-builtin-operative
    (blambda ,lambda-list ,eparam ,fparam ,@body) ',(isymify name)))

(defmacro defop (name lambda-list eparam fparam &body body)
  `(let ((op (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body)))
     (i:define op ',(isymify name) *defining-environment*)
     (push (cons ',(isymify name)
                 (make-instance 'cenv:binding
                   :info (make-instance 'info:known-operative
                           :dynenvp ,(not (eq eparam 'ignore))
                           :value op)))
           *defining-compilation-bindings*)))

(defmacro defapp (name lambda-list eparam fparam &body body)
  `(let ((op (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body)))
     (i:define (i:wrap op) ',(isymify name) *defining-environment*)
     (push (cons ',(isymify name)
                 (make-instance 'cenv:binding
                   :info (info:wrap
                          (make-instance 'info:known-operative
                            :dynenvp ,(not (eq eparam 'ignore))
                            :value op))))
           *defining-compilation-bindings*)))

;;; Convert a Lisp boolean to a Burke one.
(defun boolify (object) (if object i:true i:false))

;;; Convenience: Define a type predicate.
(defmacro defpred (name lisp-name)
  `(defapp ,name (object) ignore ignore (boolify (,lisp-name object))))

(defmacro defmac (name lambda-list eparam fparam &body body)
  `(let ((op (burke-operative ,name ,lambda-list ,eparam ,fparam ,@body)))
     (i:define (make-macro op) ',(isymify name) *defining-environment*)
     (push (cons ',(isymify name)
                 (make-instance 'cenv:binding
                   :info (make-instance 'info:macro :expander op)))
           *defining-compilation-bindings*)))
