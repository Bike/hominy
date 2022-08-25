(in-package #:burke/treec)

(defgeneric convert-known-operation (name combiner-node combinand env-var cenv))

(defmethod convert-known-operation (name combinern combinand env-var cenv)
  ;; In general, give up- let convert-combination do its default.
  (declare (ignore name combinern combinand env-var cenv))
  nil)

(defmethod convert-known-operation ((name (eql 'syms::$if))
                                    combinern combinand envv cenv)
  (destructuring-bind (condition then else) combinand
    (make-if (convert-form condition envv cenv)
             (convert-form then envv cenv)
             (convert-form else envv cenv))))

(defmethod convert-known-operation ((name (eql 'syms::$vau))
                                    combinern combinand envv cenv)
  ;; We have to include the combinern since it might have side effects.
  ;; In the usual case that it doesn't because it's something basic like a symbol,
  ;; the backend will compile it down to nothing.
  (make-seq
   (list combinern)
   (destructuring-bind (plist eparam . body) combinand
     (let ((op (convert-operative plist eparam body cenv)))
       ;; See note on defclass enclose for some explanation.
       (if (closes-env-p op)
           (make-enclose op envv)
           op)))))

(defmethod convert-known-operation ((name (eql 'syms::$sequence))
                                    combinern combinand envv cenv)
  (make-seq
   (list combinern)
   (convert-seq combinand envv cenv)))

(defmethod convert-known-operation ((name (eql 'syms::$let))
                                    combinern combinand envv cenv)
  (destructuring-bind (bindings . body) combinand
    (make-seq
     (list combinern)
     (cond ((null bindings)
            ;; ($let () ...) = ($sequence ...), except for the new empty child environment.
            ;; I think Burke's semantics ought to allow this equivalency. In particular,
            ;; with mutable bindings, it should be possible to mutate the bindings of mutable
            ;; parent environments, if not to add more bindings.
            ;; The only other problem is that if the dynamic environment is _not_ fixed,
            ;; $let creates a fixed environment, so that would be detectable. But in the
            ;; compiler we can avoid that problem, since we are compiling an operative, and
            ;; thus all the environments involved are fixed, starting with the local.
            (convert-seq body envv cenv))
           (t
            (let* ((new-env-var (make-symbol "LET-ENVIRONMENT"))
                   (plists (mapcar #'first bindings))
                   (valns (mapcar (lambda (bind)
                                    (convert-form (second bind) envv cenv))
                                  bindings))
                   (new-bindings (reduce #'append bindings
                                         :key (lambda (binding)
                                                (plist->bindings (first binding)))))
                   (cenv (cenv:augment1 cenv new-bindings))
                   (body (convert-seq body new-env-var cenv))
                   (free (free body))
                   (reifiedp (member new-env-var free))
                   ;; If the new-env-var is free, we need the parent env-var too.
                   ;; Also, remove any variables that are actually bound here.
                   (really-free (nset-difference (if reifiedp (list* envv free) free)
                                                 (mapcar #'car new-bindings)))
                   ;; If it's not free, record that by storing the letn's env-var as nil.
                   (penv (if reifiedp new-env-var nil)))
              (make-instance 'letn
                :plists plists :value-nodes valns :env-var envv :inner-env-var penv
                :free really-free :body body)))))))
