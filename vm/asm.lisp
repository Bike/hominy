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
   ;; Currently assigned position in the module.
   (%position :accessor cf-position :type (integer 0))
   (%annotations :initform nil :accessor annotations)
   ;; General entry point: Two args are the combinand and the dynamic environment.
   (%gep :reader gep :type label :initform (make-label))
   ;; Call entry point: Args are the dynenv, followed by the 1st element of the
   ;; combinand, the second, etc. Good for applicatives.
   (%cep :reader cep :type label :initform (make-label))
   ;; Local entry point: Args are ignored. The frame is set up in some
   ;; combiner-specific way.
   (%lep :reader lep :type label :initform (make-label))
   (%nlocals :initform 0 :accessor nlocals :type (and unsigned-byte fixnum))
   (%nstack :initform 0 :accessor nstack :type (and unsigned-byte fixnum))
   (%closed :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader closed
            :type (array t (*)))
   (%name :initform nil :initarg :name :accessor name)))

;;; A thing we need to note somehow during final assembly.
(defclass annotation ()
  (;; The function the annotation is in.
   (%cfunction :initarg :cfunction :accessor annotation-cfunction)
   ;; The position in the bytecode that the annotation affects,
   ;; i.e. the position that will need some kind of editing/resolution.
   ;; Relative to the cfunction.
   (%position :initform nil :initarg :position :accessor annotation-position
              :type (or null (integer 0)))))

(defun module-position (annotation)
  (+ (cf-position (annotation-cfunction annotation))
     (annotation-position annotation)))

(defun annotate (cfunction annotation)
  (assert (annotation-position annotation))
  (setf (annotations cfunction)
        (merge 'list (list annotation) (annotations cfunction) #'<
               :key #'annotation-position)))

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
(defclass label (annotation) ())

;;; A marker for something to resolve during linking.
(defclass fixup (annotation) ())

;;; A fixup that refers to a label: the position needs to be replaced with
;;; wherever the label ends up.
;;; (There might be other fixups later for like deciding if something needs a cell?)
(defclass label-fixup (fixup)
  ((%label :initarg :label :reader label :type label)))

(defun make-label () (make-instance 'label))

(defun assemble (cfunction &rest items)
  (loop with bytecode = (bytecode cfunction)
        for item in items
        do (etypecase item
             (symbol ; treat as an instruction name
              (vector-push-extend (symbol-value item) bytecode))
             (label
              ;; We're referencing a label, so insert a 0, and record a fixup.
              (annotate cfunction
                        (make-instance 'label-fixup
                          :cfunction cfunction :label item
                          :position (vector-push-extend 0 bytecode))))
             ((unsigned-byte 8) ; literal constant
              (vector-push-extend item bytecode)))))

;;; Mark where a label is in the cfunction.
(defun emit-label (cfunction label)
  (setf (annotation-cfunction label) cfunction
        (annotation-position label) (nbytes cfunction)))

(defun resolve-annotations (cfunction)
  (dolist (annotation (annotations cfunction))
    (when (typep annotation 'fixup)
      (resolve-fixup annotation))))

(defgeneric resolve-fixup (fixup))
(defmethod resolve-fixup ((fixup label-fixup))
  (let* ((label (label fixup))
         (cfunction (annotation-cfunction label))
         (fpos (annotation-position fixup))
         (pos (module-position fixup))
         (target (module-position label))
         ;; -1 because jumps are relative to the opcode, not the label.
         (diff (- target pos -1)))
      (if (typep diff '(signed-byte 7))
          (setf (aref (bytecode cfunction) fpos) (ldb (byte 8 0) diff))
          (error "Diff too big: ~d" diff))))

;;; Assign initial module positions to the cfunctions.
;;; With multi-byte labels, these could be shifted a little during linking.
(defun initialize-cfunction-positions (cfunctions)
  (loop for pos = 0 then (+ pos (length (bytecode cf)))
        for cf in cfunctions
        do (setf (cf-position cf) pos)))

;;; Produce a vm:module and vm:codes from cfunction and its module, by resolving any
;;; unresolved labels (when that's a thing), concatenating the bytecode vector, etc.
;;; If the cfunction closes over anything, RESOLVE is called on each element of the
;;; assembler closed vector to produce the values. Otherwise a CODE is returned
;;; directly.
(defun link (cfunction)
  (let* ((cmodule (cmodule cfunction))
         (cfunctions (cfunctions cmodule))
         (_ (initialize-cfunction-positions cfunctions))
         (bytecode-length (reduce #'+ cfunctions :key (lambda (c) (length (bytecode c)))))
         (bytecode (make-array bytecode-length :element-type '(unsigned-byte 8)))
         (cconstants (constants cmodule))
         (nconstants (length cconstants))
         (constants (make-array nconstants))
         (module (make-instance 'vm:module
                   :bytecode bytecode :constants constants))
         (codes
           (loop for fstart = 0 then fend
                 for cfunction in cfunctions
                 for fbytecode = (bytecode cfunction)
                 for fend = (+ fstart (length fbytecode))
                 do (resolve-annotations cfunction)
                    (replace bytecode fbytecode :start1 fstart)
                 collect (make-instance 'vm:code
                           :module module
                           :gep (module-position (gep cfunction))
                           :cep (module-position (cep cfunction))
                           :lep (module-position (lep cfunction))
                           :end fend :nregs (nlocals cfunction) :nstack (nstack cfunction)
                           :nclosed (length (closed cfunction)) :name (name cfunction)))))
    (declare (ignore _))
    (loop for i below nconstants
          for cconst = (aref cconstants i)
          do (setf (aref constants i)
                   (if (typep cconst 'cfunction)
                       (nth (position cconst cfunctions) codes)
                       cconst)))
    (nth (position cfunction cfunctions) codes)))
