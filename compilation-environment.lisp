(in-package #:burke/cenv)

;;; Info about a variable binding.
;;; This is an object instead of just the info because compilers may
;;; subclass it to store other, compiler-specific information.
(defclass binding ()
  ((%info :initarg :info :initform (info:default-info) :reader info :type info:info)))

(defclass cenvironment ()
  (;; A list of CENVIRONMENTs.
   (%parents :initarg :parents :reader parents :type list)
   ;; An alist (symbol . binding)
   (%bindings :initarg :bindings :reader bindings :type list)
   ;; A boolean indicating whether this environment may have additional, unknown bindings.
   ;; Note that this does not include any parent environments, i.e. a complete
   ;; cenvironment may have incomplete ancestors.
   (%completep :initarg :completep :initform t :reader completep :type boolean)))

(defun empty-cenv ()
  (make-instance 'cenvironment :parents nil :bindings nil))

(defun make-cenv (parents completep &rest bindings)
  (make-instance 'cenvironment
    :parents parents :bindings bindings :completep completep))

;;; Do a simple augmentation - complete, only one parent.
(defun augment1 (parent bindings)
  (if (null bindings)
      parent
      (make-instance 'cenvironment
        :parents (list parent) :completep t :bindings bindings)))

;; Look up the name in the cenv. Return a binding if there is or may be one,
;; otherwise NIL.
(defun lookup (symbol cenv)
  (let ((pair (assoc symbol (bindings cenv))))
    (cond (pair (cdr pair))
          ((completep cenv)
           ;; depth first search, like interpreter environments
           ;; note we can return NIL here.
           (some (lambda (cenv) (lookup symbol cenv)) (parents cenv)))
          (t ; could be anything
           (make-instance 'binding :info (info:default-info))))))
