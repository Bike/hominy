(in-package #:burke/treec)

;;;; Produce IR from source forms.

(defclass local-binding (cenv:binding) ())
(defun make-local-binding () (make-instance 'local-binding))

(defun ptree->bindings (ptree)
  (labels ((aux (ptree)
             (etypecase ptree
               ((or null i:ignore) nil)
               (symbol (list (cons ptree (make-local-binding))))
               (cons (append (aux (car ptree)) (aux (cdr ptree)))))))
    (aux ptree)))

;;; Return a binding alist for make-cenv.
(defun operative-bindings (ptree eparam)
  (let ((binds (ptree->bindings ptree)))
    (etypecase eparam
      (symbol (list* (cons eparam (make-local-binding)) binds))
      (i:ignore binds))))

(defun convert-operative (ptree eparam body static-env-var cenv)
  (let* ((bindings (operative-bindings ptree eparam))
         (cenv (cenv:augment1 cenv bindings))
         (env-var (make-symbol (format nil "LOCAL-ENVIRONMENT ~s ~a"
                                       ptree eparam)))
         (bodyn (convert-seq body env-var cenv))
         (free (free bodyn)))
    (multiple-value-bind (really-free static-env-var)
        (let* ((env-var-free-p (member env-var free))
               (free-vars (nset-difference free (mapcar #'car bindings)))
               (really-free (if env-var-free-p
                                (list* static-env-var (delete env-var free-vars))
                                free-vars)))
          (values really-free (if env-var-free-p static-env-var nil)))
      (make-instance 'operative
        :ptree ptree :eparam eparam
        :free really-free :static-env-var static-env-var :env-var env-var
        :body bodyn))))

(defun convert-seq (forms env-var cenv)
  (cond ((null forms) (convert-constant i:inert env-var cenv))
        ((null (cdr forms)) (convert-form (car forms) env-var cenv))
        (t
         (make-seq (mapcar (lambda (f) (convert-form f env-var cenv)) (butlast forms))
                   (convert-form (first (last forms)) env-var cenv)))))

(defun convert-form (form env-var cenv)
  (typecase form
    (symbol (convert-symbol form env-var cenv))
    (cons (convert-cons form env-var cenv))
    (t (convert-constant form env-var cenv))))

(defun convert-constant (value env-var cenv)
  (declare (ignore env-var cenv))
  (make-const value))

(defun convert-symbol (symbol env-var cenv)
  (declare (ignore env-var))
  (let ((binding (cenv:lookup symbol cenv)))
    (etypecase binding
      (local-binding (make-ref symbol (cenv:info binding)))
      (cenv:binding
       ;; If it's not a local binding, it must be "global", i.e. in the static
       ;; environment of whatever operative we are overall compiling.
       ;; This is only true provided we aren't trying to convert forms in any
       ;; remotely exotic environments - e.g. from eval, $remote-eval, etc.
       (make-link symbol (cenv:info binding)))
      (null
       (unless binding (warn "Unknown variable ~a" symbol))
       (make-link symbol)))))

(defun convert-cons (form env-var cenv)
  (convert-combination (convert-form (car form) env-var cenv)
                       (make-const (cdr form))
                       env-var cenv))

(defun inline-local-combination (combinern combinandn env-var)
  (let* ((body (body combinern))
         (bfree (free body))
         (eparam (eparam combinern))
         (inner-reified-p (static-env-var combinern))
         ;; If the eparam exists and is free in the body, we gotta bind it.
         ;; Also gotta bind it if the operative needs a reified inner environment.
         (dynenv-bind-p (and (symbolp eparam)
                             (or inner-reified-p (member eparam bfree)))))
    (make-instance 'letn
      :ptrees (list (ptree combinern)) :value-nodes (list combinandn)
      :dynenv-bind (if dynenv-bind-p eparam nil)
      :inner-env-var (if inner-reified-p (env-var combinern) nil)
      ;; If we need to bind the dynenv, it's free.
      ;; This is true even if the operative doesn't need a reified static environment:
      ;; because we have a vau form, the static and dynamic environments are one and the same,
      ;; and we're getting at the dynamic environment without consing up a local environment.
      :free (union (free combinandn)
                   (if dynenv-bind-p
                       (list* env-var (free combinern))
                       (free combinern)))
      :static-env-var env-var :body body)))

;;; Used as an argument to COMBINE opcode when we know the environment is not needed.
;;; This facilitates the very important optimization of not referring to the local
;;; environment in combinations that are known not to need it, which allows the local
;;; environment to go unreified.
(defvar *empty-env* (i:make-fixed-environment #() #()))

(defgeneric %convert-combination (combiner-info combiner-node combinand-node env-var cenv))

(defmethod %convert-combination (combineri combinern combinandn env-var cenv)
  (declare (ignore cenv))
  ;; Default method is that since we don't know enough, make a general combination.
  ;; But check if we don't need the dynenv. If we don't, put in a sham.
  ;; The main advantage of this is that FREE will find the env-var is not used.
  (make-combination combinern combinandn
                    (if (info:dynenvp combineri)
                        (make-ref env-var)
                        (make-const *empty-env*))))

(defmethod %convert-combination ((combineri info:known-operative) combinern combinandn env-var cenv)
  (or (and (typep combinandn 'const)
           (convert-known-operation (info:value combineri)
                                    combinern (value combinandn) env-var cenv))
      (and (typep combinandn 'listn)
           (convert-known-application (info:value combineri)
                                      combinern (elements combinandn) env-var cenv))
      (call-next-method)))

(defmethod %convert-combination ((combineri info:local-operative) combinern combinandn env-var cenv)
  (declare (ignore cenv))
  (cond ((typep combinern 'operative)
         ;; The combiner is a locally defined one, and what's more, this is the direct
         ;; and only use of it. So we can just inline it as a let node.
         ;; This is not the most general or efficient way to accomplish this, obviously.
         ;; For example, we could recompile the operative's body with respect to any
         ;; information gained from the known combinand, or we could handle more general
         ;; cases (but see below).
         (inline-local-combination combinern combinandn env-var))
        ((and (typep combinern 'seq)
              (typep (final combinern) 'operative))
         ;; This handles the common case of e.g. ($vau ...) becoming ($seq $vau ...).
         (make-seq (for-effect combinern)
                   (inline-local-combination (final combinern) combinandn env-var)))
        (t (call-next-method))))

(defmethod %convert-combination ((combineri info:applicative) combinern combinandn env-var cenv)
  (if (typep combinandn 'const)
      (convert-combination (make-unwrap combinern)
                           (make-listn
                            (loop for form in (value combinandn)
                                  collect (convert-form form env-var cenv)))
                           env-var cenv)
      (call-next-method)))

(defun expand-macro (expander combinand)
  (multiple-value-bind (expansion error)
      ;; FIXME: IGNORE-ERRORS may not be ideal
      ;; wrt NLX within burke.
      (ignore-errors (ctcombine expander combinand))
    (cond (error
           (warn "~a while macroexpanding: ~a" 'error error)
           (values nil nil))
          (t (values expansion t)))))

(defmethod %convert-combination ((combineri info:macro) combinern combinandn env-var cenv)
  (if (typep combinandn 'const)
      (multiple-value-bind (expansion expandedp)
          (expand-macro (info:expander combineri) (value combinandn))
        (if expandedp
            (convert-form expansion env-var cenv)
            (call-next-method)))
      (call-next-method)))

(defmethod %convert-combination ((combineri info:constant)
                                 combinern combinandn env-var cenv)
  (let ((value (info:value combineri)))
    (typecase value
      (i:macro
       (if (typep combinandn 'const)
           (multiple-value-bind (expansion expandedp)
               (expand-macro (i:expander value) (value combinandn))
             (if expandedp
                 (convert-form expansion env-var cenv)
                 (call-next-method)))
           (call-next-method)))
      (i:operative
       (or (and (typep combinandn 'const)
                (convert-known-operation value combinern (value combinandn)
                                         env-var cenv))
           (and (typep combinandn 'listn)
                (convert-known-application value combinern (elements combinandn)
                                           env-var cenv))
           (call-next-method)))
      (i:applicative
       (if (typep combinandn 'const)
           (convert-combination
            (make-const (i:unwrap value))
            (make-listn
             (loop for form in (value combinandn)
                   collect (convert-form form env-var cenv)))
            env-var cenv)
           (call-next-method)))
      (t (call-next-method)))))

(defun convert-combination (combinern combinandn env-var cenv)
  (%convert-combination (info combinern) combinern combinandn env-var cenv))
