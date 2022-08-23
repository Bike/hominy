(in-package #:burke/treec)

;;; Abstract class for IR for a form.
(defclass node () ())

(defgeneric info (node))

(defclass operative (node)
  ((%plist :initarg :plist :reader plist)
   (%eparam :initarg :eparam :reader eparam)
   (%body :initarg :body :reader body :type node)
   ;; A list of free symbols.
   (%free :initarg :free :reader operative-free :type list)
   ;; True iff the operative uses its static environment arbitrarily
   ;; (i.e. if it needs to be reified by the encloser)
   (%closes-env-p :initarg :closes-env-p :reader closes-env-p :type boolean)
   ;; A symbol uses internally to refer to the local environment.
   (%env-var :initarg :env-var :reader env-var :type symbol)))
(defmethod info ((node operative))
  ;; TODO
  (info:default-info))

;;; Indicator that an operative needs a reified environment.
;;; This essentially exists so that FREE can get at the appropriate env-var.
;;; A lack of ENCLOSE does not mean that the operative won't have to be a closure,
;;; just that it doesn't have to close over its static environment.
(defclass enclose (node)
  ((%operative :initarg :operative :reader operative :type operative)
   (%env-var :initarg :env-var :reader env-var :type symbol)))
(defun make-enclose (operative env-var)
  (make-instance 'enclose :operative operative :env-var env-var))
(defmethod info ((node enclose)) (info (operative node)))

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

;; Production of a list of arguments for an applicative combination.
(defclass listn (node)
  ((%elements :initarg :elements :reader elements :type list)))
(defun make-listn (elements) (make-instance 'listn :elements elements))
(defmethod info ((node listn)) (info:default-info)) ; TODO

(defclass unwrap (node)
  ((%applicative :initarg :applicative :reader applicative :type node)))
(defun make-unwrap (app) (make-instance 'unwrap :applicative app))
(defmethod info ((node unwrap)) (info:unwrap (info (applicative node))))

(defclass seq (node)
  (;; A list of nodes for the forms evaluated for-effect.
   (%for-effect :initarg :for-effect :reader for-effect :type list)
   ;; The node for the last form, that provides the value for the seq.
   (%final :initarg :final :reader final :type node)))
(defun make-seq (for-effect final)
  (make-instance 'seq :for-effect for-effect :final final))
(defmethod info ((node seq)) (info (final node)))

(defclass ifn (node)
  ((%condition :initarg :condition :reader if-cond :type node)
   (%then :initarg :then :reader then :type node)
   (%else :initarg :else :reader else :type node)))
(defun make-if (cond then else) (make-instance 'ifn :condition cond :then then :else else))
(defmethod info ((node ifn)) (info:default-info)) ; TODO