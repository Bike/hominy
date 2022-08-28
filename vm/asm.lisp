(in-package #:burke/vm/asm)

;;; A module being compiled.
(defclass cmodule ()
  ((%cfunctions :initform nil :accessor cfunctions :type list)
   (%constants :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader constants)
   (%annotations :initform nil :accessor annotations)))

(defgeneric constant-index (thing value))
(defmethod constant-index ((cmodule cmodule) value)
  (let ((constants (constants cmodule)))
    (or (position value constants)
        (vector-push-extend value constants))))

;;; A function (operative) being compiled.
(defclass cfunction ()
  ((%cmodule :initarg :cmodule :initform (error "missing arg") :reader cmodule :type cmodule)
   (%ptree :initarg :ptree :initform (error "missing arg") :reader ptree)
   (%eparam :initarg :eparam :initform (error "missing arg") :reader eparam)
   (%bytecode :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader bytecode)
   (%annotations :initform nil :accessor annotations)
   (%sep :accessor sep :type (and unsigned-byte fixnum)) ; xep is 0
   (%nlocals :initform 0 :accessor nlocals :type (and unsigned-byte fixnum))
   (%nstack :initform 0 :accessor nstack :type (and unsigned-byte fixnum))
   (%closed :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader closed
            :type (array t (*)))
   (%name :initform nil :initarg :name :accessor name)))

;;; A thing we need to note somehow during final assembly.
(defclass annotation ()
  (;; The position in the bytecode that the annotation affects, i.e. the position that will
   ;; need some kind of editing/resolution.
   (%position :initform nil :accessor annotation-position :type (or null (integer 0)))))

(defun annotate (cfunction annotation)
  (assert (annotation-position annotation))
  (setf (annotations cfunction)
        (merge 'list (list annotation) (annotations cfunction) #'< :key #'annotation-position)))

(defmethod initialize-instance :after ((inst cfunction) &key cmodule (name nil namep)
                                       &allow-other-keys)
  (declare (ignore name))
  ;; Add the cfunction to the module.
  (push inst (cfunctions cmodule))
  ;; Put in a default name if none was provided.
  (unless namep
    (setf (name inst) `(burke/interpreter/syms::$vau ,(ptree inst) ,(eparam inst)))))

(defun closure-index (cfunction thing)
  (or (position thing (closed cfunction))
      (vector-push-extend thing (closed cfunction))))
(defmethod constant-index ((cfunction cfunction) value)
  (constant-index (cmodule cfunction) value))

(defun nbytes (cfunction) (length (bytecode cfunction)))

;;; A reference into the code vector that can't be resolved until later.
;;; TODO: Variable size labels and stuff. (Scary.)
(defclass label (annotation)
  (;; The position the label is at.
   (%target :initform nil :accessor label-target)))

(defun make-label (cfunction)
  (declare (ignore cfunction))
  (make-instance 'label))

(defun assemble (cfunction &rest items)
  (loop with bytecode = (bytecode cfunction)
        for item in items
        do (etypecase item
             (symbol ; treat as an instruction name
              (vector-push-extend (symbol-value item) bytecode))
             (label
              (assert (null (annotation-position item))) ; no dupe assembly
              ;; Now that the label has actually been positioned, add it to the module
              ;; so the linker can resolve it later, and of course also set the
              ;; position that will need the resolution.
              (setf (annotation-position item) (vector-push-extend 0 bytecode))
              (annotate cfunction item))
             ((unsigned-byte 8) ; literal constant
              (vector-push-extend item bytecode)))))

(defun emit-label (cfunction label) (setf (label-target label) (nbytes cfunction)))

(defun resolve-annotations (cfunction)
  (dolist (annotation (annotations cfunction))
    (assert (typep annotation 'label))
    (let* ((fixup (annotation-position annotation))
           (target (label-target annotation))
           ;; -1 because jumps are relative to the opcode, not the label.
           (diff (- target fixup -1)))
      (if (typep diff '(signed-byte 7))
          (setf (aref (bytecode cfunction) fixup) (ldb (byte 8 0) diff))
          (error "Diff too big: ~d" diff)))))

;;; Produce a vm:module and vm:codes from cfunction and its module, by resolving any
;;; unresolved labels (when that's a thing), concatenating the bytecode vector, etc.
;;; If the cfunction closes over anything, RESOLVE is called on each element of the
;;; assembler closed vector to produce the values. Otherwise a CODE is returned
;;; directly.
(defun link (cfunction)
  (let* ((cmodule (cmodule cfunction))
         (cfunctions (cfunctions cmodule))
         (bytecode-length (reduce #'+ cfunctions :key (lambda (c) (length (bytecode c)))))
         (bytecode (make-array bytecode-length :element-type '(unsigned-byte 8)))
         (cconstants (constants cmodule))
         (nconstants (length cconstants))
         (constants (make-array nconstants))
         (module (make-instance 'vm:module
                   :bytecode bytecode :constants constants))
         (codes
           (loop for xep-start = 0 then fend
                 for cfunction in cfunctions
                 for fbytecode = (bytecode cfunction)
                 for fend = (+ xep-start (length fbytecode))
                 do (resolve-annotations cfunction)
                    (replace bytecode fbytecode :start1 xep-start)
                 collect (make-instance 'vm:code
                           :module module :xep xep-start :sep (+ xep-start (sep cfunction))
                           :end fend :nregs (nlocals cfunction) :nstack (nstack cfunction)
                           :nclosed (length (closed cfunction)) :name (name cfunction)))))
    (loop for i below nconstants
          for cconst = (aref cconstants i)
          do (setf (aref constants i)
                   (if (typep cconst 'cfunction)
                       (nth (position cconst cfunctions) codes)
                       cconst)))
    (nth (position cfunction cfunctions) codes)))
