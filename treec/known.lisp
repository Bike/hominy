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
