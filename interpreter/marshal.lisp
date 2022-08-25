(in-package #:burke/interpreter)

(cpk:defencoding regular-environment %parents %table)
(cpk:defencoding fixed-environment %parents %names %vvec)
(cpk:defencoding applicative %underlying)

;;; The augmenter is a function, so we can't dump it. Instead, recompute when loading.
(defmethod cpk:encode-object append ((object derived-operative) &key &allow-other-keys)
  (cpk:slots-to-alist (object) %ptree %eparam %env %body))
(defmethod cpk:decode-object-initialize progn ((object derived-operative) class alist
                                               &key &allow-other-keys)
  (declare (cl:ignore class))
  (cpk:alist-to-slots (alist object) %ptree %eparam %env %body)
  (setf (augmenter object)
        (make-augmenter (cdr (assoc '%env alist))
                        (cdr (assoc '%ptree alist)) (cdr (assoc '%eparam alist)))))

;;; This is a sham put in so that conspack can check that builtins don't have circularity.
;;; Builtin operatives cannot actually be dumped.
(defmethod cpk:encode-object append ((object builtin-operative) &key &allow-other-keys)
  nil)

;;; These also need to be linked, but always, so it's easier.
(defmethod cpk:encode-object append ((object ignore) &key &allow-other-keys) nil)
(defmethod cpk:encode-object append ((object inert) &key &allow-other-keys) nil)
(defmethod cpk:encode-object append ((object boolean) &key &allow-other-keys)
  (list (cons '%value (value object))))
(defmethod cpk:decode-object-allocate ((class (eql 'ignore)) alist &key &allow-other-keys)
  (declare (cl:ignore alist))
  ignore)
(defmethod cpk:decode-object-allocate ((class (eql 'inert)) alist &key &allow-other-keys)
  (declare (cl:ignore alist))
  inert)
(defmethod cpk:decode-object-allocate ((class (eql 'boolean)) alist &key &allow-other-keys)
  (let ((pair (assoc '%value alist)))
    (cond ((not pair) (error "BOOLEAN serialization is buggy: alist ~a" alist))
          ((cdr pair) true)
          (t false))))
(defmethod cpk:decode-object-initialize progn
    ((obj ignore) class alist &key &allow-other-keys)
  (declare (cl:ignore class alist)))
(defmethod cpk:decode-object-initialize progn
    ((obj inert) class alist &key &allow-other-keys)
  (declare (cl:ignore class alist)))
(defmethod cpk:decode-object-initialize progn
    ((obj boolean) class alist &key &allow-other-keys)
  (declare (cl:ignore class alist)))

;;; Do we care about linking this? We don't if reconstructing an eq? object is easy, like
;;; it is for numbers or symbols.
(defun trivial-object-p (object)
  (typep object '(or boolean ignore inert symbol number)))

;;; Go through the environment recording any nontrivial objects we might need to link.
(defun compute-link-table (environment)
  (let ((table (make-hash-table))) ; FIXME test by coalescability?
    (labels ((note (symbol value)
               (unless (trivial-object-p value)
                 (setf (gethash value table) symbol)))
             (aux (env)
               (map-bindings #'note env)
               (map-parents #'aux env)))
      (aux environment)
      ;; 0 is a special marker meaning to map to the linkage environment itself.
      ;; FIXME: Better marker? The point is it's not a symbol, so it's distinct from
      ;; all the symbols in the map we just computed.
      (setf (gethash environment table) 0))
    table))

(defun marshal (object linkenv)
  (let ((link-table (compute-link-table linkenv)))
    (cpk:with-linking ((lambda (o) (nth-value 1 (gethash o link-table)))
                       (lambda (o) (gethash o link-table)))
      (cpk:tracking-refs ()
        (cpk:encode object)))))

(defun marshal-to-file (object linkenv savfile)
  (let ((link-table (compute-link-table linkenv)))
    (with-open-file (s savfile :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create
                               :if-exists :supersede)
      (cpk:with-linking ((lambda (o) (nth-value 1 (gethash o link-table)))
                         (lambda (o) (gethash o link-table)))
        (cpk:tracking-refs ()
          (cpk:encode object :stream s))))))

(defun unmarshal (bytevec linkenv)
  (cpk:tracking-refs ()
    (cpk:with-remote-refs (lambda (ref)
                            (etypecase ref
                              ((eql 0) linkenv)
                              (symbol (lookup ref linkenv))))
      (cpk:decode bytevec))))

(defun unmarshal-from-file (savfile linkenv)
  (with-open-file (s savfile :element-type '(unsigned-byte 8))
    (cpk:tracking-refs ()
      (cpk:with-remote-refs (lambda (ref)
                              (etypecase ref
                                ((eql 0) linkenv)
                                (symbol (lookup ref linkenv))))
        (cpk:decode-stream s)))))
