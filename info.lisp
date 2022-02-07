(in-package #:burke/flow)

;; An INFO is something passed around by flow analysis (data or control).
;; In order to run something like Kildall, they must support meet and join
;; operations, as well as a subinfop to determine if propagation should
;; continue.
;; Info are computed from other info, or from constants and parameters and
;; encloses and stuff.
;; FOLLOWING NOT ACTUALLY TRUE AT LEAST YET:
;; What flow analysis actually passes around is a combination of several kinds
;; of info, each accessible with a different reader. Flow computations can
;; use multiple kinds of datum but only output one, i.e. the function that
;; computes types is different from the one that computes environment bindings.

(defclass info ()
  ((%type :initarg :type :reader type)))

(defun default-info () (make-instance 'info :type (type:top)))

(defgeneric join/2 (info1 info2)
  (:method ((i1 info) (i2 info))
    (make-instance 'info :type (type:disjoin (type i1) (type i2)))))
(defgeneric meet/2 (info1 info2)
  (:method ((i1 info) (i2 info))
    (make-instance 'info :type (type:conjoin (type i1) (type i2)))))
(defgeneric subinfop (sub super)
  (:method ((i1 info) (i2 info))
    (type:subtypep (type i1) (type i2))))
