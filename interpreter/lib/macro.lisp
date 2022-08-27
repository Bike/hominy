(in-package #:burke/interpreter)

(defclass macro (operative)
  ((%expander :initarg :expander :reader expander :type combiner)))
(defun make-macro (expander) (make-instance 'macro :expander expander))
(defmethod name ((op macro)) (name (expander op)))

(defmethod combine ((combiner macro) combinand env)
  (eval (combine (expander combiner) combinand env) env))

(defenv *macro* ()
  (defapp make-macro (expander) ignore (make-macro expander))
  (defapp macro-expander (macro) ignore (expander macro))
  (let (($vau (lookup 'syms::$vau *ground*))
        ;; KLUDGE?
        (make-macro (lookup 'syms::make-macro *defining-environment*)))
    (defmac $macro (ptree eparam &rest body) ignore
      (list make-macro (list* $vau ptree eparam body)))))
