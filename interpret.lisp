(in-package #:burke)

(defclass ignore () ())
(defun ignorep (object) (typep object 'ignore))
(defmethod print-object ((object ignore) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "#ignore" stream)))

(defclass inert () ())
(defun inertp (object) (typep object 'inert))
(defmethod print-object ((object inert) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "#inert" stream)))

(defun read-\#i (stream dispchar num)
  (declare (cl:ignore dispchar num))
  (flet ((check-char (char)
           (let ((c (read-char stream)))
             (unless (char= c char)
               (error "#i misspelling")))))
    (let ((next (read-char stream)))
      (ecase next
        ((#\g) (map nil #'check-char "nore") (make-instance 'ignore))
        ((#\n) (map nil #'check-char "ert") (make-instance 'inert))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass environment () ())
(defun environmentp (object) (typep object 'environment))

(declaim (ftype (function (environment) (or environment null)) parent))
(defgeneric map-parents (function environment)
  (:argument-precedence-order environment function))
(defgeneric local-lookup (symbol environment)
  (:argument-precedence-order environment symbol)
  (:documentation "Check this environment (and not parents) for a value.
If it exists, returns VALUE T; otherwise NIL NIL."))
(defgeneric define (new symbol environment)
  (:argument-precedence-order environment symbol new)
  (:documentation "Modify an existing binding, or create a new one.
Note that it is not possible to create or modify bindings in a parent."))

(defun lookup (symbol environment)
  "Find the value bound to SYMBOL in ENVIRONMENT.
An environment with multiple parents is searched in depth-first order, as specified by Kernel.
Signals an error if the symbol is not bound in the environment."
  (labels ((aux (environment)
             (multiple-value-bind (value presentp)
                 (local-lookup symbol environment)
               (if presentp
                   (return-from lookup value)
                   (map-parents #'aux environment)))))
    (aux environment)
    (error "Unbound variable ~a" symbol)))

;;; An environment that allows mutability, suitable for global environments.
;;; This can be used like standard Kernel's environments.
;;; Implemented as a hash table of cell objects.
(defclass regular-environment (environment)
  ((%parents :initarg :parents :reader parents)
   (%table :initform (make-hash-table :test #'eq)
           :reader regular-environment-table)))

(defun cell (symbol regular-environment)
  (gethash symbol (regular-environment-table regular-environment)))
(defun (setf cell) (new symbol regular-environment)
  (setf (gethash symbol (regular-environment-table regular-environment)) new))

(defmethod map-parents (function (env regular-environment))
  (mapc function (parents env)))
(defmethod local-lookup (symbol (env regular-environment))
  (multiple-value-bind (cell presentp) (cell symbol env)
    (if presentp (values (car cell) t) (values nil nil))))
(defmethod define (new symbol (env regular-environment))
  (multiple-value-bind (cell presentp) (cell symbol env)
    (if presentp
        (setf (car cell) new)
        (setf (cell symbol env) (list new))))
  new)

(defun make-environment (&rest parents)
  (make-instance 'regular-environment :parents parents))

;;; An environment with a fixed set of bindings, suitable for LET and etc.
;;; FIXME: Could also have cells. Honestly cells should probably be a totally
;;; separate thing?
(defclass fixed-environment (environment)
  (;; Obviously LET doesn't produce an environment with multiple parents, but
   ;; it shouldn't be especially hard to support someone doing that if they
   ;; really want to for whatever reason.
   (%parents :initarg :parents :reader parents)
   (%names :initarg :names :reader names :type (simple-array symbol (*)))
   (%vvec :initarg :vvec :reader vvec :type simple-vector)))

(defmethod map-parents (function (env fixed-environment))
  (mapc function (parents env)))
(defmethod local-lookup (symbol (env fixed-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))
(defmethod define (new symbol (env fixed-environment))
  (let ((pos (position symbol (names env))))
    (if pos
        (setf (aref (vvec env) pos) new)
        (error "New bindings cannot be added to a fixed environment ~a" env))))

(defun %augment (env names values)
  (make-instance 'fixed-environment
    :parents (list env)
    :names (coerce names 'vector) :vvec (coerce values 'vector)))

(defun augment (env &rest bindings)
  (loop for (name value) on bindings by #'cddr
        collect name into names
        collect value into values
        finally (return (%augment env names values))))

(defun make-fixed-environment (symbols values &rest parents)
  (make-instance 'fixed-environment
    :parents parents
    :names (coerce symbols 'vector)
    :vvec (coerce values 'vector)))

(defun $define! (env name form) (define (eval form env) name env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric eval (form env))
(defgeneric combine (combiner combinand env))

(defmethod eval ((form symbol) env) (lookup form env))
(defmethod eval ((form cons) env)
  (combine (eval (car form) env) (cdr form) env))
(defmethod eval ((form null) env) (declare (cl:ignore env)) form)
(defmethod eval ((form t) env) (declare (cl:ignore env)) form)

(defun evaluator (env) (lambda (form) (eval form env)))
(defun evlis (forms env) (mapcar (evaluator env) forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass combiner () ())
(defclass operative (combiner) ())
(defclass builtin-operative (operative)
  ((%fun :initarg :fun :reader builtin-impl :type function)))
(defclass derived-operative (operative)
  ((%plist :initarg :plist :reader plist) 
   (%eparam :initarg :eparam :reader eparam :type (or ignore symbol))
   (%env :initarg :env :reader env)
   ;; A function that, given the dynamic environment and combinand, returns a
   ;; new environment to evaluate the body in. PLIST, EPARAM, and ENV are only
   ;; there for introspection/completeness/whatever.
   (%augmenter :initarg :augmenter :reader augmenter :type function)
   ;; A list of forms (not just one form)
   (%body :initarg :body :reader body)))
(defclass applicative (combiner)
  ((%underlying :initarg :underlying :reader unwrap :type combiner)))

(defun operativep (object) (typep object 'operative))
(defun applicativep (object) (typep object 'applicative))
(defun wrap (combiner) (make-instance 'applicative :underlying combiner))

(defmethod combine ((combiner builtin-operative) combinand env)
  (funcall (builtin-impl combiner) env combinand))
(defmethod combine ((combiner derived-operative) combinand env)
  (apply #'$sequence
         (funcall (augmenter combiner) env combinand)
         (body combiner)))
(defmethod combine ((combiner applicative) combinand env)
  (combine (unwrap combiner) (evlis combinand env) env))

(defun %plist-augmenter (plist start)
  (etypecase plist
    (ignore (values nil (lambda (combinand vec)
                          (declare (cl:ignore combinand vec)
                                   ;; hopefully prevents argcount check
                                   (optimize speed (safety 0))))))
    (null (values nil (lambda (combinand vec)
                        (declare (cl:ignore vec) (optimize speed (safety 0)))
                        (unless (null combinand)
                          (error "Too many arguments")))))
    (symbol (values (list plist)
                    (lambda (combinand vec)
                      ;; prevent argcount check and vector length check
                      (declare (optimize speed (safety 0)))
                      (setf (svref vec start) combinand))))
    (cons (multiple-value-bind (car-names car-augmenter)
              (%plist-augmenter (car plist) start)
            (multiple-value-bind (cdr-names cdr-augmenter)
                (%plist-augmenter (cdr plist) (+ start (length car-names)))
              (declare (type (function (t simple-vector))
                             car-augmenter cdr-augmenter))
              (values (append car-names cdr-names)
                      (lambda (combinand vec)
                        ;; hopefully this will prevent car-augmenter from
                        ;; checking argcounts and such too
                        (declare (optimize speed (safety 0)))
                        (typecase combinand
                          (cons
                           (funcall car-augmenter (car combinand) vec)
                           (funcall cdr-augmenter (cdr combinand) vec))
                          (t (error "Not enough arguments"))))))))))

(defun make-augmenter (static-env plist eparam)
  (etypecase eparam
    (ignore
     (multiple-value-bind (names augmenter) (%plist-augmenter plist 0)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce names 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (declare (cl:ignore dynamic-env))
           (let ((vvec (make-array nnames)))
             (funcall augmenter combinand vvec)
             (%augment static-env names-vec vvec))))))
    (symbol
     (multiple-value-bind (names augmenter) (%plist-augmenter plist 1)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce (list* eparam names) 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (let ((vvec (make-array nnames)))
             (setf (svref vvec 0) dynamic-env)
             (funcall augmenter combinand vvec)
             (%augment static-env names-vec vvec))))))))

(defun $vau (static-env plist eparam &rest body)
  (let ((aug (make-augmenter static-env plist eparam)))
    (make-instance 'derived-operative
      :plist plist :eparam eparam :env static-env :augmenter aug
      ;; Used to do (cons '$sequence body) here, but then $sequence becoming
      ;; rebound would be an issue, so instead the evaluator (above) has been
      ;; modified to do a sequence of forms directly.
      :body body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass boolean ()
  ((%value :initarg :value :reader value :type (member t nil))))

(defmethod print-object ((object boolean) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "#~c" (if (value object) #\t #\f))))

(defun booleanp (object) (typep object 'boolean))

(defun $if (env condition then else)
  (let ((c (eval condition env)))
    (cond ((not (booleanp c)) (error "Invalid condition for ~s: ~s" '$if c))
          ((value c) (eval then env))
          (t (eval else env)))))

(defun read-#t (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  (make-instance 'boolean :value t))

(defun read-#f (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  (make-instance 'boolean :value nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Kernel defines some of these as derived, but some of these are really dang
;;; involved that way, like $sequence, or having to derive them considerably
;;; confuses other definitions, as with $let.

(defun $sequence (env &rest forms)
  (cond ((null forms) (make-instance 'inert))
        ((null (rest forms)) (eval (first forms) env))
        (t (loop for (form . rest) on forms
                 for value = (eval form env)
                 when (null rest)
                   return value))))

(defun $let (env bindings &rest body)
  (let* ((names (mapcar #'first bindings))
         (forms (mapcar #'second bindings))
         (values (map 'vector (evaluator env) forms))
         (new-env (%augment env names values)))
    (apply #'$sequence new-env body))
  #+(or)
  (let ((body (wrap (apply #'$vau env (mapcar #'first bindings)
                           (make-instance 'ignore)
                           forms))))
    (combine body (mapcar #'second bindings) env)))

;;; Given our fixed environments, this actually can't be derived.
;;; It also has slightly different behavior from Kernel with respect to forms
;;; that immediately evaluate the newly bound names. In Kernel, doing such will
;;; get you the outside binding value if there is one, or else error with an
;;; unbound variable. (This is not stated outright but is the behavior of the
;;; given derivation.) This here binds everything to #inert. I think the ideal
;;; would be to signal an error. To do that, either there needs to be a special
;;; "unbound" marker to put in temporarily, or something like symbol macros.
;;; I'm inclined to the latter.
(defun $letrec (env bindings &rest body)
  (let* ((names (mapcar #'first bindings))
         (forms (mapcar #'second bindings))
         (inert (make-instance 'inert))
         (vals (make-array (length names) :initial-element inert))
         (new-env (%augment env names vals)))
    ;; This is a little nasty - we directly mutate the value vector which is
    ;; now stored in the environment. If %augment started copying, we'd have
    ;; some issues.
    (map-into vals (lambda (form) (eval form new-env)) forms)
    (apply #'$sequence new-env body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kar (cons)
  (if (consp cons)
      (car cons)
      (error 'type-error :expected-type 'cons :datum cons)))
(defun kdr (cons)
  (if (consp cons)
      (cdr cons)
      (error 'type-error :expected-type 'cons :datum cons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-ground (env)
  (labels ((simp (f) (lambda (dynamic-env combinand)
                       (apply f dynamic-env combinand)))
           (ign (f) (lambda (dynamic-env combinand)
                      (declare (cl:ignore dynamic-env))
                      (apply f combinand)))
           (op (f) (make-instance 'builtin-operative :fun f))
           (app (f) (make-instance 'applicative :underlying (op f))))
    ;; core semantics
    (define (app (ign #'eval)) 'eval env)
    (define (app (ign #'combine)) 'combine env)
    (define (app (ign #'lookup)) 'lookup env)
    ;; ignores
    (define (app (ign #'ignorep)) 'ignore? env)
    ;; environments
    (define (app (ign #'environmentp)) 'environment? env)
    (define (app (ign #'make-environment)) 'make-environment env)
    (define (app (ign #'make-fixed-environment)) 'make-fixed-environment env)
    (define (op (simp #'$define!)) '$define! env)
    ;; operatives
    (define (op (simp #'$vau)) '$vau env)
    (define (app (ign #'operativep)) 'operative? env)
    ;; applicatives
    (define (app (ign #'applicativep)) 'applicative? env)
    (define (app (ign #'wrap)) 'wrap env)
    (define (app (ign #'unwrap)) 'unwrap env)
    ;; lists
    (define (app (ign #'cons)) 'cons env)
    (define (app (ign #'kar)) 'car env) ; incorrect, CL:CAR works on nil
    (define (app (ign #'kdr)) 'cdr env)
    (define (app (ign #'consp)) 'cons? env) ; "pair?" in kernel
    (define (app (ign #'null)) 'null? env)
    ;; symbols
    (define (app (ign #'symbolp)) 'symbol? env)
    ;; equivalence
    (define (app (ign #'eql)) 'eq? env)
    ;; booleans
    (define (op (simp #'$if)) '$if env)
    (define (app (ign #'booleanp)) 'boolean? env)
    ;; control
    (define (op (simp #'$sequence)) '$sequence env)
    (define (op (simp #'$let)) '$let env))
  env)

(defun install-reader-macros (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\i #'read-#i readtable)
  (set-dispatch-macro-character #\# #\t #'read-#t readtable)
  (set-dispatch-macro-character #\# #\f #'read-#f readtable)
  (values))

(defun repl ()
  (let* ((*readtable* (copy-readtable nil))
         (*package* (find-package "BURKE"))
         (ground (make-environment))
         (repl-env (make-environment ground)))
    (initialize-ground ground)
    (install-reader-macros)
    (loop (format t "~&> ")
          (format t "~a~%" (eval (read) repl-env)))))
