(in-package #:burke/interpreter)

(defclass macro (operative)
  ((%expander :initarg :expander :reader expander :accessor %expander :type combiner)))
(defun make-macro (expander) (make-instance 'macro :expander expander))
(defmethod name ((op macro)) (name (expander op)))

(declaim (inline make-macroexpansion-frame))
(defstruct (macroexpansion-frame (:include frame)
                                 (:constructor make-macroexpansion-frame
                                     (parent env)))
  env)
(defmethod combine ((combiner macro) combinand env &optional frame)
  (eval (let ((frame (make-macroexpansion-frame frame env)))
          (declare (dynamic-extent frame))
          (combine (expander combiner) combinand env frame))
        env frame))
(defmethod continue ((frame macroexpansion-frame) value)
  (continue
   (frame-parent frame)
   (eval value (macroexpansion-frame-env frame) (frame-parent frame))))

(defenv *macro* ()
  (defapp make-macro (expander) ignore ignore (make-macro expander))
  (defapp macro-expander (macro) ignore ignore (expander macro))
  (let (($vau (lookup 'syms::$vau *ground*))
        ;; KLUDGE?
        (make-macro (lookup 'syms::make-macro *defining-environment*)))
    (defmac $macro (ptree eparam &rest body) ignore ignore
      (list make-macro (list* $vau ptree eparam body)))))
