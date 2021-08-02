(in-package #:burke)

(defvar *vars*)
(defvar *temps*)

(defun var (datum)
  (or (gethash datum *vars*)
      (setf (gethash datum *vars*)
            (if (ignorep datum)
                (gensym "IGNORED")
                (make-symbol (symbol-name (name datum)))))))

(defun temp (datum)
  (or (gethash datum *vars*)
      (let ((v (var datum))) ; make new variable
        (push v *temps*)
        v)))

;;; Read an existing variable.
(defgeneric dvar (datum))
(defmethod dvar ((datum constant)) `',(value datum))
(defun guaranteed-var (datum)
  (or (gethash datum *vars*)
      (error "BUG: Missing datum ~a" datum)))
(defmethod dvar ((datum argument)) (guaranteed-var datum))
(defmethod dvar ((datum enclosed)) (guaranteed-var datum))
(defmethod dvar ((datum instruction)) (guaranteed-var datum))

(defun %plist-to-dbind (plist)
  (etypecase plist
    (null (values nil nil))
    (ignore (let ((i (var plist))) (values i (list i))))
    (argument (let ((v (var plist))) (values v nil)))
    (cons (multiple-value-bind (v1 i1)
              (plist-to-dbind (car plist))
            (multiple-value-bind (v2 i2)
                (plist-to-dbind (cdr plist))
              (values (cons v1 v2) (append i1 i2)))))))

(defun plist-to-dbind (plist)
  (multiple-value-bind (db ignore) (%plist-to-dbind plist)
    (if (and (symbolp db) (not (null db)))
        (values `(&rest ,db) ignore)
        (values db ignore))))

(defun ins (instruction)
  (loop for use in (inputs instruction)
        collect (dvar (definition use))))

(defgeneric translate-instruction (instruction))
(defmethod translate-instruction ((instruction lookup))
  `(setq ,(temp instruction) (lookup ,@(ins instruction))))
(defmethod translate-instruction ((instruction combination))
  ;; FIXME: inefficient
  (destructuring-bind (combiner env rest &rest args) (ins instruction)
    `(setq ,(temp instruction) (combine ,combiner (list* ,@args ,rest) ,env))))
(defmethod translate-instruction ((instruction ret))
  `(return ,@(ins instruction)))

(defun translate-cblock (cblock)
  (let ((code nil))
    (map-cblock-instructions (lambda (i)
                               (push (translate-instruction i) code))
                             cblock)
    (nreverse code)))

(defun ir2cl (cfunction)
  (declare (optimize debug))
  (let ((*vars* (make-hash-table :test #'eq))
        (*temps* nil))
    (multiple-value-bind (db ign) (plist-to-dbind (plist cfunction))
      (let* ((enclosed-vars (mapcar #'var (encloseds cfunction)))
             (envvar (var (eargument cfunction)))
             (body ; TODO: multiple blocks
               (translate-cblock (start cfunction))))
        `(lambda (,@enclosed-vars ,envvar combinand)
           (declare (ignorable ,(var (eargument cfunction))))
           (destructuring-bind ,db combinand
             (declare (ignorable ,@ign))
             (prog (,@*temps*)
                (declare (ignorable ,@*temps*))
                ;; TODO: multiple blocks
                ,@body)))))))
