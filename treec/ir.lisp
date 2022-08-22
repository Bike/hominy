(in-package #:burke/treec)

;;; Abstract class for IR for a form.
(defclass node () ())

(defgeneric info (node))

(defclass operative (node)
  ((%plist :initarg :plist :reader plist)
   (%eparam :initarg :eparam :reader eparam)
   (%body :initarg :body :reader body :type node)
   ;; Either a list of free lvars, or a single lvar. In the latter case, what is meant
   ;; is that the environment itself is used freely, and the lvar is bound to it.
   (%free :initarg :free :reader operative-free :type (or lvar list))))
(defmethod info ((node operative))
  ;; TODO
  (info:default-info))

;; Reference to a "global" variable, i.e. one not bound within the operative being
;; compiled, i.e. from its static environment.
(defclass link (node)
  ((%symbol :initarg :symbol :reader link-symbol :type node)
   (%info :initarg :info :reader info :type info:info)))
(defun make-link (symbol &optional (info (info:default-info)))
  (make-instance 'link :info info :symbol symbol))

(defclass ref (node)
  ((%symbol :initarg :symbol :reader ref-symbol :type symbol)
   (%info :initarg :info :reader info :type info:info)))
(defun make-ref (symbol &optional (info (info:default-info)))
  ;; We could have more specific info due to e.g. local declarations.
  (make-instance 'ref :symbol symbol :info info))

(defclass const (node)
  ((%value :initarg :value :reader value)))
(defun make-const (value)
  (make-instance 'const :value value))
(defmethod info ((node const))
  (make-instance 'info:constant :value (value node)))

;; A general(ly) unknown combination.
(defclass combination (node)
  ((%combiner :initarg :combiner :reader combiner :type node)
   (%combinand :initarg :combinand :reader combinand :type node)
   (%env :initarg :env :reader env :type node)))
(defun make-combination (combiner combinand env)
  (make-instance 'combination
    :combiner combiner :combinand combinand :env env))
(defmethod info ((node combination))
  ;; TODO?
  (info:default-info))

(defclass seq (node)
  (;; A list of nodes for the forms evaluated for-effect.
   (%for-effect :initarg :for-effect :reader for-effect :type list)
   ;; The node for the last form, that provides the value for the seq.
   (%final :initarg :final :reader final :type node)))
(defun make-seq (for-effect final)
  (make-instance 'seq :for-effect for-effect :final final))
(defmethod info ((node seq)) (info (final node)))
