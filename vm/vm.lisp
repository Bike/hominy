(in-package #:burke/vm)

(declaim (inline %make-frame))
(defstruct (frame (:include i:frame)
                  (:constructor %make-frame (parent registers stack)))
  (registers (error "missing arg") :type simple-vector :read-only t)
  (stack (error "missing arg") :type simple-vector :read-only t)
  (sp 0 :type (and fixnum unsigned-byte)))

;;; TODO: i:continue method (will need ip)

(declaim (inline make-frame))
(defun make-frame (parent regsize stacksize)
  (%make-frame parent (make-array regsize) (make-array stacksize)))

(defun spush (object frame)
  (setf (aref (frame-stack frame) (frame-sp frame)) object)
  (incf (frame-sp frame)))
(defun spop (frame) (aref (frame-stack frame) (decf (frame-sp frame))))
;; Get the last N pushes as a list.
(defun gather (n frame)
  (loop repeat n
        for i from (- (frame-sp frame) n)
        collect (aref (frame-stack frame) i)
        finally (decf (frame-sp frame) n)))

(defun reg (frame i) (aref (frame-registers frame) i))
(defun (setf reg) (object frame i) (setf (aref (frame-registers frame) i) object))

(defclass module ()
  ((%bytecode :initarg :bytecode :reader bytecode
              :type (simple-array (unsigned-byte 8) (*)))
   (%constants :initarg :constants :reader constants :type simple-vector)))

(defclass code (i:operative)
  ((%module :initarg :module :reader module :type module)
   ;; General entry point: Two args are the combinand and the dynamic environment.
   (%gep :initarg :gep :reader gep :type (and unsigned-byte fixnum))
   ;; Call entry point: Args are the dynenv, followed by the 1st element of the
   ;; combinand, the second, etc. Good for applicatives.
   (%cep :initarg :cep :reader cep :type (and unsigned-byte fixnum))
   ;; Local entry point: Args are ignored. The frame is set up in some
   ;; combiner-specific way.
   (%lep :initarg :lep :reader lep :type (and unsigned-byte fixnum))
   ;; Index into the bytecode where this function ends. Used for debugging.
   (%end :initarg :end :reader end :type (and unsigned-byte fixnum))
   ;; Size of the register file needed for this operative.
   (%nregs :initarg :nregs :reader nregs :type (and unsigned-byte fixnum))
   ;; ...and the stack.
   (%nstack :initarg :nstack :reader nstack :type (and unsigned-byte fixnum))
   ;; How many slots does a closure need?
   (%nclosed :initarg :nclosed :reader nclosed :type (and unsigned-byte fixnum))
   ;; Human-readable (but symbolic) name for debugging.
   (%name :initform nil :initarg :name :reader i:name)))

(defclass closure (i:operative)
  ((%code :initarg :code :reader code)
   (%closed :initarg :closed :reader closed :type simple-vector)))
(defmethod i:name ((o closure)) (i:name (code o)))

(defun enclose (code closed)
  (make-instance 'closure :code code :closed (coerce closed 'simple-vector)))

(defun vm (bytecode frame closure constants args &key (ip 0))
  (declare (optimize debug)
           (type (simple-array (unsigned-byte 8) (*)) bytecode)
           (type frame frame)
           (type simple-vector closure constants)
           (type (and unsigned-byte fixnum) ip))
  (flet ((code () (aref bytecode ip))
         (next-code () (aref bytecode (incf ip)))
         (label ()
           ;; Treat it as an sb7.
           ;; TODO: Longer labels
           (let ((raw (aref bytecode (+ ip 1))))
             (if (logbitp 7 raw)
                 (- raw #x100)
                 raw)))
         (reg (i) (reg frame i))
         ((setf reg) (object i) (setf (reg frame i) object))
         (closure (i) (aref closure i))
         (constant (i) (aref constants i))
         (spush (object) (spush object frame))
         (spop () (spop frame))
         (gather (n) (gather n frame)))
    (loop
      (ecase (code)
        ((#.o:nop) (incf ip))
        ((#.o:drop) (spop) (incf ip))
        ((#.o:dup) (let ((o (spop))) (spush o) (spush o)) (incf ip))
        ((#.o:ref) (spush (reg (next-code))) (incf ip))
        ((#.o:set) (setf (reg (next-code)) (spop)) (incf ip))
        ((#.o:closure) (spush (closure (next-code))) (incf ip))
        ((#.o:const) (spush (constant (next-code))) (incf ip))
        ((#.o:arg) (spush (nth (next-code) args)) (incf ip))
        ((#.o:listify-args) (spush (nthcdr (next-code) args)) (incf ip))
        ((#.o:check-arg-count-=)
         (let ((expected (next-code)))
           (unless (= expected (length args))
             (error "Argcount mismatch: Expected ~d, got ~d"
                    expected (length args))))
         (incf ip))
        ((#.o:check-arg-count->=)
         (let ((expected (next-code)))
           (unless (= expected (length args))
             (error "Argcount mismatch: Expected ~d, got ~d"
                    expected (length args))))
         (incf ip))
        ((#.o:make-cell) (spush (i:make-cell (spop))) (incf ip))
        ((#.o:cell-ref) (spush (i:cell-value (spop))) (incf ip))
        ((#.o:cell-set) (let ((val (spop))) (setf (i:cell-value (spop)) val))
         (incf ip))
        ((#.o:cons) (let ((cdr (spop))) (spush (cons (spop) cdr))) (incf ip))
        ((#.o:list) (spush (gather (next-code))) (incf ip))
        ((#.o:car) (spush (car (the cons (spop)))) (incf ip))
        ((#.o:cdr) (spush (cdr (the cons (spop)))) (incf ip))
        ((#.o:return) (return-from vm (spop)))
        ((#.o:jump) (incf ip (label)))
        ((#.o:jump-if-true)
         (let ((label (label)))
           (if (eql (spop) i:true)
               (incf ip label)
               (incf ip 2)))) ; skip label
        ((#.o:combine)
         (let ((env (spop)) (combinand (spop)) (combiner (spop)))
           (spush (i:combine combiner combinand env)))
         (incf ip))
        ((#.o:tail-combine)
         ;; this isn't much of a tail combination - the lisp may not be smart enough
         ;; to axe VM's stack frame before combining. FIXME
         (let ((env (spop)) (combinand (spop)) (combiner (spop)))
           (return-from vm (i:combine combiner combinand env))))
        ((#.o:lookup)
         (let ((env (spop)) (sym (spop))) (spush (i:lookup sym env)))
         (incf ip))
        ((#.o:unwrap) (spush (i:unwrap (spop))) (incf ip))
        ((#.o:enclose)
         (let ((code (constant (next-code))))
           (spush (enclose code (gather (nclosed code)))))
         (incf ip))
        ((#.o:make-environment)
         (let ((names (constant (next-code))))
           (spush (i:make-fixed-environment-with-cells
                   names (gather (length names)) (spop))))
         (incf ip))
        ((#.o:call)
         (let ((args (gather (next-code))) (combiner (spop)))
           (spush (apply #'i:call combiner args)))
         (incf ip))
        ((#.o:tail-call)
         (let ((args (gather (next-code))) (combiner (spop)))
           (return-from vm (apply #'i:call combiner args))))
        ;; call, tail-call
        ((#.o:err-if-not-cons)
         (let ((object (spop)))
           (unless (consp object) (error 'type-error :datum object :expected-type 'cons)))
         (incf ip))
        ((#.o:err-if-not-null)
         (let ((object (spop)))
           (unless (null object) (error 'type-error :datum object :expected-type 'null)))
         (incf ip))
        ((#.o:err-if-not-bool)
         (let ((object (spop)))
           (unless (typep object 'i:boolean)
             (error 'type-error :datum object :expected-type 'i:boolean)))
         (incf ip))))))

(defun vm-call (code closed args start-ip frame)
  (let ((module (module code))
        (frame (make-frame frame (nregs code) (nstack code))))
    (declare (dynamic-extent frame))
    (vm (bytecode module) frame closed (constants module) args :ip start-ip)))

;;; CODEs can be used directly as combiners iff they are not closures.
(defmethod i:combine ((combiner code) combinand env &optional frame)
  (assert (zerop (nclosed combiner)))
  (vm-call combiner #() (list combinand env) (gep combiner) frame))

(defmethod i:combine ((combiner closure) combinand env &optional frame)
  (let ((code (code combiner)))
    (vm-call code (closed combiner) (list combinand env) (gep code) frame)))

(defmethod i:call ((combiner code) env frame &rest combinand)
  (vm-call combiner #() (list* env combinand) (cep combiner) frame))

(defmethod i:call ((combiner closure) env frame &rest combinand)
  (let ((code (code combiner)))
    (vm-call code (closed combiner) (list* env combinand) (cep code) frame)))
