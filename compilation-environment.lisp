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

(defun make-cenv (completep &rest bindings)
  (make-instance 'cenvironment
    :parents nil :bindings bindings :completep completep))

;; FIXME: Needs more information, e.g. about argument counts
;; and environment usage. and of course just more names.
(defun make-standard-cenv ()
  (make-instance 'cenvironment
    :parents nil :completep nil
    :bindings (flet ((sym (name)
                       (intern name "BURKE/INTERPRETER/SYMS"))
                     (bi (info) (make-instance 'binding :info info))
                     (ko (sym &optional (dynenvp t))
                       (make-instance 'info:known-operative
                         :value (i:lookup sym i:*BASE*) :dynenvp dynenvp))
                     (ka (sym &optional (dynenvp t))
                       (info:wrap
                        (make-instance 'info:known-operative
                          :value (i:unwrap (i:lookup sym i:*BASE*))
                          :dynenvp dynenvp)))
                     (km (sym)
                       (make-instance 'info:macro
                         :expander (i:expander (i:lookup sym i:*BASE*)))))
                (append
                 ;; known operatives
                 (mapcar (lambda (name)
                           (let ((sym (sym name)))
                             (cons sym (bi (ko sym)))))
                         '("$IF" "$VAU"  "$DEFINE!" "$SET!" "$SEQUENCE"
                           "$LET"))
                 ;; known macros
                 (mapcar (lambda (name)
                           (let ((sym (sym name)))
                             (cons sym (bi (km sym)))))
                         '("$LAMBDA" "$COND" "$LET*" "$LETREC" "$LETREC*"
                           "$LET/EC"))
                 ;; known applicatives
                 (mapcar (lambda (name)
                           (let ((sym (sym name)))
                             (cons sym (bi (ka sym nil)))))
                         '("EVAL" "COMBINE" "LOOKUP"
                           "ENVIRONMENT?" "MAKE-ENVIRONMENT"
                           "MAKE-FIXED-ENVIRONMENT"
                           "OPERATIVE?" "APPLICATIVE?"
                           "WRAP" "UNWRAP" "CONS" "CAR" "CDR" "CONS?" "LIST"
                           "NULL?" "SYMBOL?" "EQ?" "BOOLEAN?" "EXIT"))))))

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

(defun module ()
  "Return a Burke environment with bindings for compilation environment names."
  (i:make-fixed-environment
   '(burke/interpreter/syms::standard-compilation-environment)
   (list (make-standard-cenv))))
