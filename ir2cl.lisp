(in-package #:burke)

(defvar *vars*)
(defvar *temps*)

(defun var (datum)
  (or (gethash datum *vars*)
      (setf (gethash datum *vars*)
            (make-symbol (symbol-name (ir:name datum))))))

(defun temp (datum)
  (or (gethash datum *vars*)
      (let ((v (var datum))) ; make new variable
        (push v *temps*)
        v)))

;;; Read an existing variable.
(defgeneric dvar (datum))
(defmethod dvar ((datum ir:constant)) `',(value datum))
(defun guaranteed-var (datum)
  (or (gethash datum *vars*)
      (error "BUG: Missing datum ~a" datum)))
(defmethod dvar ((datum ir:function)) (guaranteed-var datum))
(defmethod dvar ((datum ir:continuation)) (guaranteed-var datum))
(defmethod dvar ((datum ir:parameter)) (guaranteed-var datum))
(defmethod dvar ((datum ir:enclosed)) (guaranteed-var datum))
(defmethod dvar ((datum ir:bind)) (guaranteed-var datum))

(defun inps (inputs) (mapcar #'dvar inputs))
(defun ins (instruction) (inps (ir:inputs instruction)))

(defgeneric translate-instruction (instruction))

(defmethod translate-instruction :around ((instruction ir:bind))
  `(setq ,(temp instruction) ,(call-next-method)))
(defmethod translate-instruction ((inst ir:lookup)) `(lookup ,@(ins inst)))
(defmethod translate-instruction ((inst ir:cons)) `(cons ,@(ins inst)))
(defmethod translate-instruction ((inst ir:car)) `(car ,@(ins inst)))
(defmethod translate-instruction ((inst ir:cdr)) `(cdr ,@(ins inst)))
(defmethod translate-instruction ((inst ir:enclose)) `(enclose ,@(ins inst)))
(defmethod translate-instruction ((inst ir:augment)) `(caugment ,@(ins inst)))

(defmethod translate-instruction :around ((inst ir:terminator))
  ;; Go to the next continuation.
  `(return (funcall ,(dvar (first (ir:inputs inst))) ,(call-next-method))))
(defmethod translate-instruction ((inst ir:combination))
  `(combine ,@(inps (rest (ir:inputs inst)))))
(defmethod translate-instruction ((inst ir:eval))
  `(eval ,@(inps (rest (ir:inputs inst)))))
(defmethod translate-instruction ((inst ir:sequence))
  `(apply #'$sequence ,(dvar (third (ir:inputs inst)))
          ,(dvar (second (ir:inputs inst)))))
(defmethod translate-instruction ((inst ir:continue))
  (dvar (second (ir:inputs inst))))

(defun translate-continuation (cont)
  (let* ((children (ir:children cont))
         (*temps* nil)
         ;; continuation fnames are put in the vars before generating any,
         ;; so that they can refer to one another.
         (cfnames
           (loop for child in children
                 for name = (make-symbol (ir:name child))
                 do (setf (gethash child *vars*) `#',name)
                 collect name))
         (body nil))
    (ir:map-instructions (lambda (i) (push (translate-instruction i) body))
                         cont)
    (setf body (nreverse body))
    `(labels (,@(loop for child in children for cfname in cfnames
                      collect `(,cfname (,(var (ir:parameter child)))
                                  ,(translate-continuation child))))
       (let (,@*temps*)
         (declare (ignorable ,@*temps*))
         ,@body))))

(defun ir2cl (cfunction)
  (declare (optimize debug))
  (let* ((*vars* (make-hash-table :test #'eq))
         (start (ir:start cfunction))
         (param (var (ir:parameter start))))
    ;; Mark the return continuation as doing nothing
    (setf (gethash (ir:rcont cfunction) *vars*) '#'identity)
    ;; Translate
    `(lambda (,(var (ir:enclosed cfunction)) ,param)
       (block nil
         ,(translate-continuation start)))))
