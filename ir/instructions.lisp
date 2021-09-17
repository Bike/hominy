(in-package #:burke/ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; particular instructions
;;;

(defclass lookup (bind) ()) ; inputs: symbol environment
(defclass cons (bind) ()) ; inputs: car cdr
(defclass car (bind) ()) ; inputs: cons
(defclass cdr (bind) ()) ; inputs: cons
(defclass enclose (bind) ()) ; inputs: function, enclosed
(defclass augment (bind) ()) ; inputs: env, plist, combinand

;;; NOTE: All terminators have their destination as first operand.
;;; Also note that the dynenv of a combination is incorporated into the
;;; argument for uniform treatment.
;; inputs: continuation, combiner, argument
(defclass combination (terminator) ())
;; inputs: continuation, combiner, enclosed, argument
(defclass local-combination (terminator) ())
(defclass eval (terminator) ()) ; continuation, form, env
(defclass sequence (terminator) ()) ; continuation, forms, env
(defclass continue (terminator) ()) ; inputs: continuation, argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readers
;;;

(macrolet ((defread (class name n)
             `(defmethod ,name ((instruction ,class))
                (definition (nth ,n (%uinputs instruction)))))
           (defreads (class &rest names)
             `(progn
                ,@(loop for name in names for i from 0
                        collect `(defread ,class ,name ,i)))))
  (defreads lookup lname env)
  (defreads cons car cdr)
  (defreads car cons)
  (defreads cdr cons)
  (defreads enclose efunction enclosed)

  (defreads combination destination combiner argument)
  (defreads local-combination destination combiner enclosed argument)
  (defreads continue destination argument)
  (defreads eval destination form env)
  (defreads sequence destination forms env))
