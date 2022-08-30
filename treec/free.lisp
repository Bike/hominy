(in-package #:burke/treec)

;;; Return a list of lvars free in NODE.
(defgeneric free (node))

;; OPERATIVE and LETN have FREE as a slot reader.
(defmethod free ((link link)) nil)
(defmethod free ((ref ref)) (list (ref-symbol ref)))
(defmethod free ((const const)) nil)
(defmethod free ((combination combination))
  (union (free (combiner combination))
         (union (free (combinand combination)) (free (env combination)))))
(defmethod free ((node listn))
  (reduce #'union (elements node) :key #'free :initial-value ()))
(defmethod free ((node unwrap)) (free (applicative node)))
(defmethod free ((seq seq))
  (union (free (final seq))
         (reduce #'union (for-effect seq) :key #'free :initial-value ())))
(defmethod free ((ifn ifn))
  (union (free (if-cond ifn)) (union (free (then ifn)) (free (else ifn)))))
#|
;;; Returns a list of lvars something in NODE closes over, i.e. that are free in some suboperative.
(defgeneric encloses
|#
