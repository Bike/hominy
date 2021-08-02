(in-package #:burke)

(defclass cfunction ()
  ((%plist :initarg :plist :accessor plist)
   ;; A (proper) list of arguments.
   (%arguments :initarg :arguments :accessor arguments)
   ;; The environment argument or IGNORE.
   (%eargument :initarg :eargument :accessor eargument)
   ;; A (proper) list of encloseds.
   (%encloseds :initarg :encloseds :accessor encloseds)
   (%start :initarg :start :accessor start :type cblock)))

(defclass cblock ()
  ((%cfunction :initarg :cfunction :accessor cfunction)
   ;; A (proper) list of arguments.
   (%arguments :initarg :arguments :accessor arguments)
   (%start :initarg :start :accessor start :type instruction)))

(defclass datum ()
  ((%name :initarg :name :initform (gensym) :reader name :type symbol)
   ;; A proper list of USEs
   (%uses :initarg :uses :initform nil :accessor uses :type list)))

(defun add-use (datum use) (push use (uses datum)))
(defun map-uses (function datum)
  (mapc function (uses datum))
  (values))

(defclass instruction (datum)
  ((%cblock :initarg :cblock :accessor cblock)
   ;; A (proper) list of USEs.
   (%inputs :initarg :inputs :accessor inputs :type list)
   (%prev :initarg :prev :accessor prev :type (or null instruction))
   (%next :initarg :next :accessor next :type (or null instruction))))

(defun map-cblock-instructions (f cblock)
  (loop for instruction = (start cblock) then (next instruction)
        until (null instruction)
        do (funcall f instruction)))

(defmethod cfunction ((o instruction)) (cfunction (cblock o)))

(defclass use ()
  ((%definition :initarg :definition :accessor definition :type datum)
   (%user :initarg :user :accessor user :type instruction)
   ;; Dataflow analysis should be able to get information specific to this
   ;; use and not the definition. Like the basic
   ;; (if (typep x 'foo) x #|wow it's a foo!!|# x #|but here it's not|#) stuff
   (%type)
   (%dx)))

(defun map-inputs (function instruction)
  (mapc function (inputs instruction))
  (values))

(defclass constant (datum)
  ((%value :initarg :value :reader value)))
(defun constant (value) (make-instance 'constant :value value))

(defclass argument (datum) ())

(defclass enclosed (datum) ())

;;; particular instructions

;;; Two inputs: symbol, environment.
(defclass lookup (instruction) ())

;;; Three or more inputs: combiner, environment, &rest, fixed arguments.
(defclass combination (instruction) ())

;;; One input.
(defclass ret (instruction) ())
