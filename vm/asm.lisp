(in-package #:burke/vm/asm)

;;; A module being compiled.
(defclass cmodule ()
  ((%cfunctions :initform nil :accessor cfunctions :type list)
   (%constants :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader constants)))

(defun constant-index (value cmodule)
  (let ((constants (constants cmodule)))
    (or (position value constants)
        (vector-push-extend value constants))))

;;;A function (operative) being compiled.
(defclass cfunction ()
  ((%cmodule :initarg :cmodule :initform (error "missing arg") :reader cmodule :type cmodule)
   (%plist :initarg :plist :initform (error "missing arg") :reader plist)
   (%eparam :initarg :eparam :initform (error "missing arg") :reader eparam)
   (%bytecode :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader bytecode)
   (%sep :accessor sep :type (and unsigned-byte fixnum)) ; xep is 0
   (%nlocals :initform 0 :accessor nlocals :type (and unsigned-byte fixnum))
   (%nstack :initform 0 :accessor nstack :type (and unsigned-byte fixnum))
   (%closed :initform (make-array 0 :fill-pointer 0 :adjustable t) :reader closed
            :type (array t (*)))))

(defmethod initialize-instance :after ((inst cfunction) &key cmodule &allow-other-keys)
  (push inst (cfunctions cmodule)))

(defun closure-index (cfunction thing)
  (or (position thing (closed cfunction))
      (vector-push-extend thing (closed cfunction))))

(defun nbytes (cfunction) (length (bytecode cfunction)))

(defun assemble (cfunction &rest items)
  (loop with bytecode = (bytecode cfunction)
        for item in items
        do (etypecase item
             (symbol ; treat as an instruction name
              (vector-push-extend (symbol-value item) bytecode))
             ((unsigned-byte 8) ; literal constant
              (vector-push-extend item bytecode)))))

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
                   :bytecode bytecode :constants (copy-seq (constants cmodule))))
         (codes
           (loop for xep-start = 0 then (+ xep-start (length fbytecode))
                 for cfunction in cfunctions
                 for fbytecode = (bytecode cfunction)
                 do (replace bytecode fbytecode :start1 xep-start)
                 collect (make-instance 'vm:code
                           :module module :xep xep-start :sep (+ xep-start (sep cfunction))
                           :nregs (nlocals cfunction) :nstack (nstack cfunction)
                           :nclosed (length (closed cfunction))))))
    (loop for i below nconstants
          for cconst = (aref constants i)
          do (setf (aref constants i)
                   (if (typep cconst 'cfunction)
                       (nth (position cconst cfunctions) codes)
                       cconst)))
    (nth (position cfunction cfunctions) codes)))
