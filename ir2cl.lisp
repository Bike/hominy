(in-package #:burke)

(defvar *vars*)
(defvar *temps*)

(defun var (datum)
  (or (gethash datum *vars*)
      (setf (gethash datum *vars*)
            (make-symbol (symbol-name (ir:name datum))))))

;;; Read an existing variable.
(defun dvar (datum)
  (or (gethash datum *vars*)
      (error "BUG: Missing datum ~a" datum)))

(defgeneric translate-datum (datum))
(defmethod translate-datum ((datum ir:constant)) `',(ir:value datum))
(defmethod translate-datum ((datum ir:function)) (dvar datum))
(defmethod translate-datum ((datum ir:continuation)) (dvar datum))
(defmethod translate-datum ((datum ir:parameter)) (dvar datum))
(defmethod translate-datum ((datum ir:enclosed)) (dvar datum))

(defvar *translation-creations*)
(defvar *translation-initializations*)

(defgeneric translate-node (node)
  (:method :around ((node ir:node))
    (if (gethash node *vars*) ; already translated
        (dvar node)
        (let ((var (var node)))
          (multiple-value-bind (create initialize) (call-next-method)
            (push `(,var ,create) *translation-creations*)
            (when initialize
              (push initialize *translation-initializations*)))
          var))))

(defmethod translate-datum ((datum ir:node)) (translate-node datum))

(defun inp (input) (translate-datum input))
(defun inps (inputs) (mapcar #'inp inputs))
(defun ins (instruction) (inps (ir:inputs instruction)))

(defmethod translate-node ((node ir:lookup)) `(lookup ,@(ins node)))
(defmethod translate-node ((node ir:cons))
  (values `(cons nil nil)
          (let ((v (dvar node)) (i (ins node)))
            `(setf (car ,v) ,(first i) (cdr ,v) ,(second i)))))
(defmethod translate-node ((node ir:car)) `(kar ,@(ins node)))
(defmethod translate-node ((node ir:cdr)) `(kdr ,@(ins node)))
(defmethod translate-node ((node ir:enclose))
  (let ((v (dvar node)) (i (ins node)))
    (values `(make-instance 'compiled-operative :fun ,(first i))
            `(setf (%enclosed ,v) ,(second i)))))
(defmethod translate-node ((node ir:augment))
  (let ((v (dvar node)) (i (ins node)))
    ;; environments can't be their own parents, so dependency here is ok.
    (values `(caugment1 ,(first i))
            `(caugment2 ,v ,@(rest i)))))

(defgeneric translate-terminator (terminator))
;;; These methods all look similar because they have only one successor.
;;; Multiple-successor terminators will look different.
(defmethod translate-terminator ((inst ir:combination))
  `(return (funcall ,(dvar (first (ir:inputs inst)))
                    (combine ,@(inps (rest (ir:inputs inst)))))))
(defmethod translate-terminator ((inst ir:eval))
  `(return (funcall ,(dvar (first (ir:inputs inst)))
                    (eval ,@(inps (rest (ir:inputs inst)))))))
(defmethod translate-terminator ((inst ir:sequence))
  `(return (funcall ,(dvar (first (ir:inputs inst)))
                    (apply #'$sequence ,(inp (third (ir:inputs inst)))
                           ,(inp (second (ir:inputs inst)))))))
(defmethod translate-terminator ((inst ir:continue))
  `(return (funcall ,(dvar (first (ir:inputs inst)))
                    ,@(inps (rest (ir:inputs inst))))))

(defun translate-continuation (cont)
  (let* ((children (ir:children cont))
         ;; continuation fnames are put in the vars before generating any,
         ;; so that they can refer to one another.
         (cfnames
           (loop for child in children
                 for name = (make-symbol (ir:name child))
                 do (setf (gethash child *vars*) `#',name)
                 collect name))
         (terminator (ir:terminator cont))
         (*translation-creations* nil)
         (*translation-initializations* nil)
         (body (translate-terminator terminator)))
    `(labels (,@(loop for child in children for cfname in cfnames
                      collect `(,cfname (,(var (ir:parameter child)))
                                        ,(translate-continuation child))))
       (let* (,@(nreverse *translation-creations*))
         ,@(nreverse *translation-initializations*)
         ,body))))

(defun ir2cl (cfunction)
  (declare (optimize debug))
  (let* ((*vars* (make-hash-table :test #'eq))
         (start (ir:start cfunction))
         (param (ir:parameter start))
         (vparam (var param))
         (enclosed (ir:enclosed cfunction))
         (venclosed (var enclosed)))
    ;; Mark the return continuation as doing nothing
    (setf (gethash (ir:rcont cfunction) *vars*) '#'identity)
    ;; Translate
    `(lambda (,venclosed ,vparam)
       ,@(when (ir:unusedp enclosed) `((declare (cl:ignore ,venclosed))))
       ,@(when (ir:unusedp param) `((declare (cl:ignore ,vparam))))
       (block nil
         ,(translate-continuation start)))))
