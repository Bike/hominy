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

(defun convert-operative (ptree eparam body cenv)
  (let* ((bindings (operative-bindings ptree eparam))
         (cenv (cenv:augment1 cenv bindings))
         (env-var (make-symbol "LOCAL-ENVIRONMENT"))
         (bodyn (convert-seq body env-var cenv))
         (free (free bodyn)))
    (multiple-value-bind (really-free closes-env-p)
        (if (member env-var free)
            (values (delete env-var (nset-difference free (mapcar #'car bindings))) t)
            (values (nset-difference free (mapcar #'car bindings)) nil))
      (make-instance 'operative
        :ptree ptree :eparam eparam
        :free really-free :closes-env-p closes-env-p :env-var env-var
        :body (convert-seq body env-var cenv)))))

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
         ;; If the eparam exists and is free in the body, we gotta bind it.
         (outer-env-bind-p (and (symbolp eparam) (member eparam bfree))))
    (make-instance 'letn
      :ptrees (list (ptree combinern)) :value-nodes (list combinandn)
      :outer-env-bind (if outer-env-bind-p eparam nil)
      :inner-env-var (if (closes-env-p combinern) (env-var combinern) nil)
      ;; If we need to bind the dynenv, it's free.
      :free (if outer-env-bind-p (list* env-var (free combinern)) (free combinern))
      :env-var env-var :body body)))

;;; Used as an argument to COMBINE opcode when we know the environment is not needed.
;;; This facilitates the very important optimization of not referring to the local
;;; environment in combinations that are known not to need it, which allows the local
;;; environment to go unreified.
(defvar *empty-env* (i:make-fixed-environment #() #()))

(defun convert-combination (combinern combinandn env-var cenv)
  (let ((combineri (info combinern)))
    (typecase combineri
      (info:known-operative
       (cond ((and (typep combinandn 'const)
                   (convert-known-operation (info:name combineri)
                                            combinern (value combinandn) env-var cenv)))
             ((not (info:dynenvp combineri))
              ;; Don't need the environment - put in a sham.
              ;; The main advantage of this is that FREE will find the env-var is not used.
              (make-combination combinern combinandn (make-const *empty-env*)))
             (t
              (make-combination combinern combinandn (make-ref env-var)))))
      (info:local-operative
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
             ;; Past this point we don't inline, because the operative could be used in
             ;; multiple positions, so inlining would involve code expansion. We should use
             ;; a more efficient call though (FIXME). 
             ((not (info:dynenvp combineri))
              (make-combination combinern combinandn (make-const *empty-env*)))
             (t
              (make-combination combinern combinandn (make-ref env-var)))))
      (info:operative
       (make-combination combinern combinandn
                         (if (info:dynenvp combineri)
                             (make-ref env-var)
                             (make-const *empty-env*))))
      (info:applicative
       (if (typep combinandn 'const)
           (convert-combination (make-unwrap combinern)
                                (make-listn
                                 (loop for form in (value combinandn)
                                       collect (convert-form form env-var cenv)))
                                env-var cenv)
           (make-combination combinern combinandn (make-ref env-var))))
      (t (make-combination combinern combinandn (make-ref env-var))))))
