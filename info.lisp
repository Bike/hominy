(in-package #:burke/info)

;; An INFO is something passed around by flow analysis (data or control).
;; In order to run something like Kildall, they must support meet and join
;; operations, as well as a subinfop to determine if propagation should
;; continue.
;; They're also used by the more basic compiler(s).
;; Info are computed from other info, or from constants and parameters and
;; encloses and stuff.
;; FOLLOWING NOT ACTUALLY TRUE AT LEAST YET:
;; What flow analysis actually passes around is a combination of several kinds
;; of info, each accessible with a different reader. Flow computations can
;; use multiple kinds of datum but only output one, i.e. the function that
;; computes types is different from the one that computes environment bindings.
;; Also not sure if it should ever be true. Not the design I settled on for
;; the abstract interpreter in Cleavir. For now the flow analysis here is
;; pretty dormant.

(defclass info ()
  ((%type :initarg :type :reader type)))

(defun default-info () (make-instance 'info :type (type:top)))

;;; Get the info you'd have from unwrapping the given info.
(defgeneric unwrap (info))
(defmethod unwrap ((info info)) (default-info))

(defclass operative (info)
  (;; Does the operative use its dynamic environment?
   (%dynenvp :initform t :initarg :dynenvp :reader dynenvp :type boolean)))

(defclass local-operative (operative)
  (;; Could be e.g. a cfunction, or other IR for the operative.
   (%data :initarg :data :reader data)))

;;; Note that we use names instead of directly some handler function etc here
;;; because these infos are used in different compilers, so we need a little
;;; bit of indirection.
(defclass known-operative (operative)
  ((%name :initarg :name :reader name)))

(defclass applicative (info)
  ((%underlying :initarg :underlying :reader unwrap)))

(defun wrap (info)
  (make-instance 'applicative :underlying info))

(defclass constant (info)
  ((%value :initarg :value :reader value)))

(defgeneric join/2 (info1 info2)
  (:method ((i1 info) (i2 info))
    (make-instance 'info :type (type:disjoin (type i1) (type i2)))))
(defgeneric meet/2 (info1 info2)
  (:method ((i1 info) (i2 info))
    (make-instance 'info :type (type:conjoin (type i1) (type i2)))))
(defgeneric subinfop (sub super)
  (:method ((i1 info) (i2 info))
    (type:subtypep (type i1) (type i2))))
