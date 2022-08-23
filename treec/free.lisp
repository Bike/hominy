(in-package #:burke/treec)

;;; Return a list of lvars free in NODE.
(defgeneric free (node))

(defmethod free ((link link)) nil)
(defmethod free ((ref ref)) (list (ref-symbol ref)))
(defmethod free ((const const)) nil)
(defmethod free ((combination combination))
  (append (free (combiner combination)) (free (combinand combination))
          (free (env combination))))
(defmethod free ((node listn))
  (reduce #'union (elements node) :key #'free))
(defmethod free ((node unwrap)) (free (applicative node)))
(defmethod free ((seq seq))
  (union (free (final seq)) (reduce #'union (for-effect seq) :key #'free)))
