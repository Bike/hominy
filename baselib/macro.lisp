(in-package #:burke/baselib)

(defclass macro (i:operative)
  ((%expander :initarg :expander :reader expander :accessor %expander :type i:combiner)))
(defun make-macro (expander) (make-instance 'macro :expander expander))
(defmethod i:name ((op macro)) (i:name (expander op)))

(declaim (inline make-macroexpansion-frame))
(defstruct (macroexpansion-frame (:include i:frame)
                                 (:constructor make-macroexpansion-frame
                                     (parent env)))
  env)
(defmethod i:combine ((combiner macro) combinand env &optional frame)
  (i:eval (let ((frame (make-macroexpansion-frame frame env)))
            (declare (dynamic-extent frame))
            (i:combine (expander combiner) combinand env frame))
          env frame))
(defmethod i:continue ((frame macroexpansion-frame) value)
  (i:continue
   (i:frame-parent frame)
   (i:eval value (macroexpansion-frame-env frame) (i:frame-parent frame))))

(defenv (*macro* *macroc*) ()
  (defapp make-macro (expander) ignore ignore (make-macro expander))
  (defapp macro-expander (macro) ignore ignore (expander macro))
  (let (($vau (i:lookup 'syms::$vau *ground*))
        ;; KLUDGE?
        (make-macro (i:lookup 'syms::make-macro *defining-environment*)))
    (defmac $macro (ptree eparam &rest body) ignore ignore
      (list make-macro (list* $vau ptree eparam body)))))
