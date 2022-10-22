(in-package #:hominy/treec)

;;; Convert an operation where the combinand is known constant.
;;; This is useful for basic operatives like $if, etc., that usually have constant combinands.
(defgeneric convert-known-operation (combiner combiner-node combinand
                                     env-var cenv))

;;; Convert an operation where the combinand is a list of known length.
;;; This is useful for basic applicatives.
(defgeneric convert-known-application (combiner combiner-node combinand-nodes
                                       env-var cenv))

(defmethod convert-known-operation (combiner combinern combinand env-var cenv)
  ;; In general, give up- let convert-combination do its default.
  (declare (ignore combiner combinern combinand env-var cenv))
  nil)
(defmethod convert-known-application (combiner combinern combinand env-var cenv)
  (declare (ignore combiner combinern combinand env-var cenv))
  nil)

(defun blookup (name) (i:lookup name baselib:*BASE*))

;;; Produce a seq node to evaluate the combiner, if we aren't sure that it's
;;; side-effect-free.
;;; This is important in order to for example reduce (unwrap (wrap x)) to x
;;; which allows known-operative optimizations. It's not the best way to do it,
;;; but as usual that will require a smarter flow-sensitive compiler.
(defun shunt-combiner (combinern bodyn)
  (if (side-effect-free-p combinern)
      bodyn
      (make-seq (list combinern) bodyn)))

(defmethod convert-known-operation ((value (eql (blookup 'syms::$if)))
                                    combinern combinand envv cenv)
  (destructuring-bind (condition then else) combinand
    (shunt-combiner
     combinern
     (make-if (convert-form condition envv cenv)
              (convert-form then envv cenv)
              (convert-form else envv cenv)))))

(defmethod convert-known-operation ((value (eql (blookup 'syms::$vau)))
                                    combinern combinand envv cenv)
  (shunt-combiner
   combinern
   (destructuring-bind (ptree eparam . body) combinand
     (convert-operative ptree eparam body envv cenv))))

(defmethod convert-known-operation ((value (eql (blookup 'syms::$sequence)))
                                    combinern combinand envv cenv)
  (shunt-combiner
   combinern
   (convert-seq combinand envv cenv)))

(defmethod convert-known-application ((value
                                       (eql (i:unwrap (blookup 'syms::unwrap))))
                                      combinern args envv cenv)
  (declare (ignore envv cenv))
  (if (= (length args) 1)
      (shunt-combiner combinern (make-unwrap (first args)))
      nil))

(defmethod convert-known-application ((value
                                       (eql (i:unwrap (blookup 'syms::wrap))))
                                      combinern args envv cenv)
  (declare (ignore envv cenv))
  (if (= (length args) 1)
      (shunt-combiner combinern (make-wrap (first args)))
      nil))

(defmethod convert-known-application ((value
                                       (eql (i:unwrap (blookup 'syms::list))))
                                      combinern args envv cenv)
  (declare (ignore envv cenv))
  (shunt-combiner combinern (make-listn args)))

(defmethod convert-known-operation ((value (eql (blookup 'syms::$set!)))
                                    combinern combinand envv cenv)
  (destructuring-bind (ptree valuef) combinand
    (shunt-combiner
     combinern
     (make-instance 'setn
       :ptree ptree
       :value (convert-form valuef envv cenv)))))
