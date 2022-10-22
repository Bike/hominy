(in-package #:hominy/treec)

;;; Abstract class for IR for a form.
(defclass node () ())

(defgeneric info (node))

;;; Used to apply simplifications.
;;; Approximate (i.e. may return false even if it is)
(defgeneric side-effect-free-p (node)
  (:method (node) (declare (ignore node)) nil))

(defclass operative (node)
  ((%ptree :initarg :ptree :reader ptree)
   (%eparam :initarg :eparam :reader eparam)
   (%body :initarg :body :reader body :type node)
   ;; A list of free symbols.
   (%free :initarg :free :reader free :type list)
   ;; True iff the operative uses its static environment arbitrarily
   ;; (i.e. if it needs to be reified by the encloser)
   ;; NIL if the operative doesn't need a reified outer environment;
   ;; otherwise the variable for the outer environment.
   (%static-env-var :initarg :static-env-var :reader static-env-var :type (or null symbol))
   ;; A symbol uses internally to refer to the local environment.
   (%env-var :initarg :env-var :reader env-var :type symbol)))
(defmethod info ((node operative))
  (make-instance 'info:local-operative
    :data node
    :dynenvp (if (static-env-var node) t nil)))
(defmethod side-effect-free-p ((node operative)) t)

;; Reference to a "global" variable, i.e. one not bound within the operative being
;; compiled, i.e. from its static environment.
(defclass link (node)
  ((%symbol :initarg :symbol :reader link-symbol :type node)
   (%info :initarg :info :reader info :type info:info)))
(defun make-link (symbol &optional (info (info:default-info)))
  (make-instance 'link :info info :symbol symbol))
(defmethod side-effect-free-p ((node link)) t)

(defclass ref (node)
  ((%symbol :initarg :symbol :reader ref-symbol :type symbol)
   (%info :initarg :info :reader info :type info:info)))
(defun make-ref (symbol &optional (info (info:default-info)))
  ;; We could have more specific info due to e.g. local declarations.
  (make-instance 'ref :symbol symbol :info info))
(defmethod side-effect-free-p ((node ref)) t)

;; A $set! form.
(defclass setn (node)
  ((%ptree :initarg :ptree :reader ptree)
   (%value :initarg :value :reader value :type node)))

(defclass const (node)
  ((%value :initarg :value :reader value)))
(defun make-const (value)
  (make-instance 'const :value value))
(defmethod info ((node const))
  (make-instance 'info:constant :value (value node)))
(defmethod side-effect-free-p ((node const)) t)

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
(defmethod side-effect-free-p ((node listn))
  (every #'side-effect-free-p (elements node)))

(defclass wrap (node)
  ((%combiner :initarg :combiner :reader unwrap :type node)))
;;; Simplifying (wrap (unwrap x)) to x might be bad, in that wrap makes a
;;; distinct object. Not sure about semantics there.
(defun make-wrap (comb) (make-instance 'wrap :combiner comb))
(defmethod info ((node wrap)) (info:wrap (info (unwrap node))))
(defmethod side-effect-free-p ((node wrap))
  (side-effect-free-p (unwrap node)))

(defclass unwrap (node)
  ((%applicative :initarg :applicative :reader applicative :type node)))
(defun make-unwrap (app)
  (if (typep app 'wrap) ; simplify
      (unwrap app)
      (make-instance 'unwrap :applicative app)))
(defmethod info ((node unwrap)) (info:unwrap (info (applicative node))))
(defmethod side-effect-free-p ((node unwrap))
  (side-effect-free-p (applicative node)))

(defclass seq (node)
  (;; A list of nodes for the forms evaluated for-effect.
   (%for-effect :initarg :for-effect :reader for-effect :type list)
   ;; The node for the last form, that provides the value for the seq.
   (%final :initarg :final :reader final :type node)))
(defun make-seq (for-effect final)
  (make-instance 'seq :for-effect for-effect :final final))
(defmethod info ((node seq)) (info (final node)))
(defmethod side-effect-free-p ((node seq))
  (and (every #'side-effect-free-p (for-effect node))
       (side-effect-free-p (final node))))

(defclass ifn (node)
  ((%condition :initarg :condition :reader if-cond :type node)
   (%then :initarg :then :reader then :type node)
   (%else :initarg :else :reader else :type node)))
(defun make-if (cond then else) (make-instance 'ifn :condition cond :then then :else else))
(defmethod info ((node ifn)) (info:default-info)) ; TODO
(defmethod side-effect-free-p ((node ifn))
  (and (side-effect-free-p (if-cond node))
       (side-effect-free-p (then node))
       (side-effect-free-p (else node))))

;;; Used to convert inline operatives, and by extension, $let.
(defclass letn (node)
  ((%ptree :initarg :ptree :reader ptree)
   (%value :initarg :value :reader value :type node)
   ;; The variable bound to the environment the let is evaluated in.
   (%static-env-var :initarg :static-env-var :reader static-env-var :type symbol)
   ;; A variable the $let will bind to the environment it is evaluated in
   ;; (for conversion of inline operatives), or NIL if not bound.
   ;; The static and dynamic environments coincide for $LET, but these two slots
   ;; have different names anyway to reflect how they originate.
   (%dynenv-bind :initform nil :initarg :dynenv-bind :reader dynenv-bind :type symbol)
   ;; A variable the body of the $let can use to refer to the environment established by the $let,
   ;; or NIL if not needed.
   (%inner-env-var :initarg :inner-env-var :reader inner-env-var :type symbol)
   (%free :initarg :free :reader free :type list)
   (%body :initarg :body :reader body :type node)))
(defmethod info ((node letn)) (info (body node)))
(defmethod side-effect-free-p ((node letn))
  (and (side-effect-free-p (value node)) (side-effect-free-p (body node))))
