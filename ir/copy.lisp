(in-package #:burke/ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR copy
;;;

;; create a copy
(defun %%copy (thing table)
  (let ((new (%allocate-copy thing)))
    (%initialize-copy thing new table)
    new))

;; look up a copy in the table if it exists, or else create one.
(defun %copy (thing table)
  (or (gethash thing table) (setf (gethash thing table) (%%copy thing table))))

;;; Create a copy for a thing but don't initialize.
(defgeneric %allocate-copy (thing))
;;; Initialize a copy.
(defgeneric %initialize-copy (old new table)
  (:method-combination progn))

(defmethod %allocate-copy ((old module)) (make-instance 'module))
(defmethod %initialize-copy progn ((old module) (new module) table)
  (map-functions (lambda (f) (add-function new (%copy f table))) old))

(defmethod %allocate-copy ((old function))
  (make-instance 'function :name (name old)))
(defmethod %initialize-copy progn ((old function) (new function) table)
  (setf (%module new) (%copy (%module old) table)
        (%rcont new) (%copy (%rcont old) table)
        (%enclosed new) (%copy (%enclosed old) table)
        (%start new) (%copy (%start old) table)))

(defmethod %allocate-copy ((old parameter))
  (make-instance 'parameter :name (name old)))
(defmethod %initialize-copy progn ((old parameter) (new parameter) table)
  (setf (%continuation new) (%copy (%continuation old) table)))

(defmethod %allocate-copy ((old enclosed))
  (make-instance 'enclosed :name (name old)))
(defmethod %initialize-copy progn ((old enclosed) (new enclosed) table)
  ;; this will have to do something if encloseds link back to function (FIXME?)
  (declare (ignore table)))

(defmethod %allocate-copy ((old constant))
  ;; for now i think we should copy this so that uses of values are local to
  ;; the function they're in
  (make-instance 'constant :name (name old) :value (value old)))
(defmethod %initialize-copy progn ((old constant) (new constant) table)
  (declare (ignore table)))

(defmethod %allocate-copy ((old continuation))
  (make-instance 'continuation :name (name old)))
(defmethod %initialize-copy progn ((old continuation) (new continuation) table)
  (setf (%parent new) (%copy (%parent old) table))
  (map-children (lambda (ch) (add-child new (%copy ch table))) old)
  (setf (%parameter new) (%copy (%parameter old) table)
        (%start new) (%copy (%start old) table)
        (%terminator new) (%copy (%terminator old) table)))

(defmethod %allocate-copy ((old instruction))
  (make-instance (class-of old) :name (name old)))
(defmethod %initialize-copy progn ((old instruction) (new instruction) table)
  (setf (%continuation new) (%copy (continuation old) table))
  (let ((prev (prev old)))
    (setf (%prev new) (if (null prev) prev (%copy prev table))))
  (setf (%uinputs new)
        (map 'list (lambda (use) (%copy use table)) (%uinputs old))))
(defmethod %initialize-copy progn ((old bind) (new bind) table)
  (setf (%next new) (%copy (%next old) table)))

(defmethod %allocate-copy ((old use)) (make-instance 'use))
(defmethod %initialize-copy progn ((old use) (new use) table)
  (setf (%definition new) (%copy (definition old) table)
        (%user new) (%copy (user old) table)))

(defgeneric copy (ir))

(defmethod copy ((module module)) (%copy module (make-hash-table)))

;;; NOTE: When copying partial structures (i.e. anything but a module), we
;;; arrange things so that there are no references to the copy. Recursive
;;; references in the substructure still refer to the original. This is so that
;;; the user of the copier can decide exactly where the copy is used.
;;; For example when inlining or loop unrolling, copying gives you one
;;; iteration, not an infinite loop of them.

(defun noncopy (object table) (setf (gethash object table) object))
(defun noncopier (table) (lambda (object) (noncopy object table)))

(defmethod copy ((function function))
  (let ((table (make-hash-table)))
    ;; Set up the table so we don't copy the module or other functions.
    (let ((mod (module function)))
      (noncopy mod table)
      (map-functions (noncopier table) mod))
    (%%copy function table)))

(defmethod copy ((cont continuation))
  (let ((table (make-hash-table)))
    ;; Insert the module, all functions, and all continuations in cont's
    ;; function into the table so that they are not copied.
    ;; (Continuations in other functions aren't directly referenced anyway.)
    (let ((mod (module (function cont)))
          (noncopier (noncopier table)))
      (noncopy mod table)
      (map-functions noncopier mod)
      (map-continuations noncopier (function cont)))
    (%%copy cont table)))
