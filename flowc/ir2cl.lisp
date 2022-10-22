(in-package #:hominy)

(defvar *vars*)
(defvar *temps*)

(defun var (datum)
  (or (gethash datum *vars*)
      (setf (gethash datum *vars*)
            (make-symbol (symbol-name (hominy/ir:name datum))))))

;;; Read an existing variable.
(defun dvar (datum)
  (or (gethash datum *vars*)
      (error "BUG: Missing datum ~a" datum)))

(defgeneric translate-datum (datum))
(defmethod translate-datum ((datum hominy/ir:constant)) `',(hominy/ir:value datum))
(defmethod translate-datum ((datum hominy/ir:function)) (dvar datum))
(defmethod translate-datum ((datum hominy/ir:continuation)) (dvar datum))
(defmethod translate-datum ((datum hominy/ir:parameter)) (dvar datum))
(defmethod translate-datum ((datum hominy/ir:enclosed)) (dvar datum))

(defvar *translation-creations*)
(defvar *translation-initializations*)

(defgeneric translate-node (node)
  (:method :around ((node hominy/ir:node))
    (if (gethash node *vars*) ; already translated
        (dvar node)
        (let ((var (var node)))
          (multiple-value-bind (create initialize) (call-next-method)
            (push `(,var ,create) *translation-creations*)
            (when initialize
              (push initialize *translation-initializations*)))
          var))))

(defmethod translate-datum ((datum hominy/ir:node)) (translate-node datum))

(defun inp (input) (translate-datum input))
(defun inps (inputs) (mapcar #'inp inputs))
(defun ins (instruction) (inps (hominy/ir:inputs instruction)))

(defmethod translate-node ((node hominy/ir:lookup))
  ;; KLUDGE
  (values nil `(setf ,(dvar node) (lookup ,@(ins node)))))
(defmethod translate-node ((node hominy/ir:cons))
  (values `(cons nil nil)
          (let ((v (dvar node)) (i (ins node)))
            `(setf (car ,v) ,(first i) (cdr ,v) ,(second i)))))
(defmethod translate-node ((node hominy/ir:car)) `(car (the cons ,@(ins node))))
(defmethod translate-node ((node hominy/ir:cdr)) `(cdr (the cons ,@(ins node))))
(defmethod translate-node ((node hominy/ir:enclose))
  (let ((v (dvar node)) (i (ins node)))
    (values `(make-instance 'compiled-operative :fun ,(first i))
            `(setf (%enclosed ,v) ,(second i)))))
(defmethod translate-node ((node hominy/ir:augment))
  (let ((v (dvar node)) (i (ins node)))
    ;; environments can't be their own parents, so dependency here is ok.
    (values `(caugment1 ,(first i))
            `(caugment2 ,v ,@(rest i)))))

(defgeneric translate-terminator (terminator))
;;; These methods all look similar because they have only one successor.
;;; Multiple-successor terminators will look different.
(defmethod translate-terminator ((inst hominy/ir:combination))
  `(return (funcall ,(dvar (first (hominy/ir:inputs inst)))
                    (ccombine ,@(inps (rest (hominy/ir:inputs inst)))))))
(defmethod translate-terminator ((inst hominy/ir:eval))
  `(return (funcall ,(dvar (first (hominy/ir:inputs inst)))
                    (i:eval ,@(inps (rest (hominy/ir:inputs inst)))))))
(defmethod translate-terminator ((inst hominy/ir:sequence))
  `(return (funcall ,(dvar (first (hominy/ir:inputs inst)))
                    (i:evalseq ,(inp (second (hominy/ir:inputs inst)))
                               ,(inp (third (hominy/ir:inputs inst)))))))
(defmethod translate-terminator ((inst hominy/ir:continue))
  `(return (funcall ,(dvar (first (hominy/ir:inputs inst)))
                    ,@(inps (rest (hominy/ir:inputs inst))))))

(defun translate-continuation (cont)
  (let* ((children (hominy/ir:children cont))
         ;; continuation fnames are put in the vars before generating any,
         ;; so that they can refer to one another.
         (cfnames
           (loop for child in children
                 for name = (make-symbol (symbol-name (hominy/ir:name child)))
                 do (setf (gethash child *vars*) `#',name)
                 collect name))
         (terminator (hominy/ir:terminator cont))
         (*translation-creations* nil)
         (*translation-initializations* nil)
         (body (translate-terminator terminator)))
    `(let* (,@(nreverse *translation-creations*))
       (labels (,@(loop for child in children for cfname in cfnames
                        collect `(,cfname (,(var (hominy/ir:parameter child)))
                                          ,(translate-continuation child))))
         ,@(nreverse *translation-initializations*)
         ,body))))

(defun ir2cl (cfunction)
  (declare (optimize debug))
  (let* ((*vars* (make-hash-table :test #'eq))
         (start (hominy/ir:start cfunction))
         (param (hominy/ir:parameter start))
         (vparam (var param))
         (enclosed (hominy/ir:enclosed cfunction))
         (venclosed (var enclosed)))
    ;; Mark the return continuation as doing nothing
    (setf (gethash (hominy/ir:rcont cfunction) *vars*) '#'identity)
    ;; Translate
    `(lambda (,venclosed ,vparam)
       ,@(when (hominy/ir:unusedp enclosed) `((declare (cl:ignore ,venclosed))))
       ,@(when (hominy/ir:unusedp param) `((declare (cl:ignore ,vparam))))
       (block nil
         ,(translate-continuation start)))))
