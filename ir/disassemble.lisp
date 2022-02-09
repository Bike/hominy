(in-package #:burke/ir)

;;;; Note that we don't use the linearization here. This is because
;;;; a) i wrote this disassembler before the linearizer, and
;;;; b) i expect the linearizer will screw up at some point, and i want to be
;;;;    able to keep reading disassemblies when it does.

(defvar *unnamed-count*)
(defvar *dis-names*)
(defgeneric dis-name (datum))
(defmethod dis-name ((datum datum))
  (or (name datum) (prog1 *unnamed-count* (incf *unnamed-count*))))
(defmethod dis-name ((datum constant)) `',(value datum))

(defgeneric disassemble-ir (ir))

(defmethod disassemble-ir ((ir instruction))
  (loop with name = (gethash ir *dis-names*)
        for i in (inputs ir)
        for v = (gethash i *dis-names*)
        if v
          collect v into inps
        else collect (setf (gethash i *dis-names*) (dis-name i)) into inps
             and nconc (disassemble-ir i) into preds
        finally
           (return (nconc preds
                          (list `(:= ,name (,(class-name (class-of ir))
                                            ,@inps)))))))

(defmethod disassemble-ir ((ir constant))
  nil)
(defmethod disassemble-ir ((ir parameter))
  nil)
(defmethod disassemble-ir ((ir enclosed))
  nil)

(defmethod disassemble-ir ((ir continuation))
  (let ((name (dis-name ir))
        (children (children ir))
        (terminator (terminator ir)))
    ;; ensure names are there
    (disassemble-ir (parameter ir))
    (loop for child in children
          do (setf (gethash child *dis-names*) (dis-name child)))
    (setf (gethash terminator *dis-names*) (dis-name terminator))
    ;; disassemble
    `(,name (,(gethash ir *dis-names*))
         (,@(mapcar #'disassemble-ir children))
       ,@(disassemble-ir terminator))))

(defmethod disassemble-ir ((ir function))
  (let* ((enclosed (enclosed ir))
         (enclosed-name (dis-name enclosed))
         (rcont (rcont ir))
         (rcont-name (dis-name rcont))
         (start (start ir)))
    (setf (gethash enclosed *dis-names*) enclosed-name
          (gethash rcont *dis-names*) rcont-name
          (gethash start *dis-names*) (dis-name start))
    `(,(gethash ir *dis-names*) (,enclosed-name ,rcont-name)
      ,(disassemble-ir start))))

(defmethod disassemble-ir ((ir module))
  (let ((fs (%functions ir)))
    (loop for function in fs
          do (setf (gethash function *dis-names*) (dis-name function)))
    (loop for function in fs
          collect (disassemble-ir function))))

(defun disassemble (module)
  (let ((*unnamed-count* 0)
        (*dis-names* (make-hash-table :test #'eq)))
    (disassemble-ir module)))
