(in-package #:burke/treec)

;;; Return a list of lvars free in NODE.
(defgeneric free (node))

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
(defmethod free ((op operative))
  ;; This whole thing kind of betrays a suboptimality of the tree compiler-
  ;; we might later find that a variable is not actually used and so we don't need
  ;; to close over it. But the tree compiler is not smart enough to realize.
  ;; Fixing this is left to the final flow compiler.
  (operative-free op))
(defmethod free ((op enclose))
  (adjoin (env-var op) (free (operative op))))
