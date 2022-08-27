(in-package #:burke/interpreter)

;;;; This file defines Kernel's keyed static variables.
;;;; These are like symbols, in that they can be bound. They are unlike symbols in that
;;;; 1) for each binding, there are distinct _binder_ and _reader_ objects
;;;; 2) they are not interned
;;;; These properties allow them to be used like gensyms for macro purposes. You can get "perfect
;;;; hygeine", as Shutt puts it, because you can give something access to the reader without
;;;; giving it access to binding, so there is no possibility of shadowing. And since they are not
;;;; interned, you don't need to worry about the reader suddenly creating a shadowing symbol.
;;;; They can also be garbage collected freely.

;;;; Unlike Shutt, rather than put by having the static keys be combiners, we introduce new types,
;;;; one for the binders and one for the readers. These can be used to implement Shutt's combiners
;;;; pretty straightforwardly.

;;;; Also unlike Shutt, we let these have names for the sake of human readability.

;;;; Possible extensions include mutating static bindings, and adding new static bindings to an
;;;; existing environment. I can't think of a purpose for the latter right this second, but I can
;;;; imagine the former being useful, again for hygeine reasons.

;;; Most of the below implementation would be considerably simplified by just using a weak-key
;;; hash table. But doing it this way is kind of educational. The optimum I guess might be some kind
;;; of perfect hashing?

;;; The reader. This will be the actual key in the environment, which does not refer to the binder.
(defstruct (static-key (:constructor %make-static-key (name)))
  (name nil :type symbol :read-only t))
;;; The binder. Contains a weak pointer to the reader.
(defstruct (static-binder (:constructor %make-static-binder (key-pointer)))
  (key-pointer (error "missing arg") :read-only t))

(defmethod print-object ((object static-key) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (static-key-name object) :stream stream))
  object)

(defmethod print-object ((object static-binder) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((key (trivial-garbage:weak-pointer-value (static-binder-key-pointer object))))
      (if key
          (write (static-key-name key) :stream stream)
          (write-string "[defunct]" stream))))
  object)

(defun make-static-key (&optional name)
  (let* ((key (%make-static-key name))
         (keyp (trivial-garbage:make-weak-pointer key))
         (binder (%make-static-binder keyp)))
    (values binder key)))

(defclass static-environment (environment) ())

(defmethod define (new (name static-key) (env static-environment))
  (declare (ignore new))
  (error "New bindings cannot be added to a static environment"))

(defclass static-fixed-environment (static-environment)
  ((%parents :initarg :parents :reader parents)
   ;; A vector of weak pointers to static keys.
   (%names :initarg :names :reader names :type simple-vector)
   (%vvec :initarg :vvec :reader vvec :type simple-vector)))

(defmethod map-parents (function (env static-fixed-environment))
  (mapc function (parents env)))
;;; Although static environments are environments, for normal evaluation purposes they are empty.
;;; LOOKUP does not work on static keys; you have to use static-lookup.
;;; This is to avoid any punning confusion, to keep the evaluation rules simple and unambiguous,
;;; and to compartmentalize static environments.
(defmethod local-lookup (symbol (env static-fixed-environment)) (values nil nil))
(defmethod map-bindings (function (env static-fixed-environment)) (declare (ignore function)))

(defgeneric local-static-lookup (key environment))

(defmethod local-static-lookup ((key static-key) (env environment)) (values nil nil))

(defmethod local-static-lookup ((name static-key) (env static-fixed-environment))
  (let ((pos (position name (names env) :key #'trivial-garbage:weak-pointer-value)))
    (if pos
        (values (aref (vvec env) pos) t)
        (values nil nil))))

(defun static-lookup (key environment)
  (labels ((aux (environment)
             (multiple-value-bind (value presentp) (local-static-lookup key environment)
               (if presentp
                   (return-from static-lookup value)
                   (map-parents #'aux environment)))))
    (aux environment)
    (error "Unbound static key ~a" key)))

(defun make-static-fixed-environment (binders nvalues &rest parents)
  (let ((names nil) (values nil))
    (map nil (lambda (binder value)
               (let* ((keyp (static-binder-key-pointer binder))
                      (key (trivial-garbage:weak-pointer-value keyp)))
                 ;; If a key has been garbage collected, nothing could possibly access a new binding,
                 ;; so don't even bother making it.
                 (when key
                   ;; Reusing a weak pointer is okay, right? I don't see why it wouldn't be.
                   (push keyp names)
                   (push value values))))
         binders nvalues)
    (let ((namevec (coerce names 'vector))
          (vvec (coerce values 'vector)))
      (let ((i 0))
        (map nil (lambda (name)
                   ;; When a name is GC'd, stomp the value to make it collectable.
                   (let ((key (trivial-garbage:weak-pointer-value name)))
                     (if key
                         (trivial-garbage:finalize
                          ;; the LET is to make sure we get a new closure.
                          name (let ((i i)) (lambda () (setf (svref vvec i) nil))))
                         ;; Wow, the key was collected while we're in the middle of trying to bind it.
                         ;; In that case, axe the value in vvec to prevent a leak.
                         (setf (svref vvec i) nil)))
                   (incf i))
             namevec))
      (make-instance 'static-fixed-environment
        :parents parents :names namevec :vvec vvec))))

;;; Like ptree-names but with static binders instead of symbols.
(defun static-ptree-names (static-ptree)
  (etypecase static-ptree
    ((or ignore null) nil)
    (static-binder (list static-ptree))
    (cons (nconc (static-ptree-names (car static-ptree)) (static-ptree-names (cdr static-ptree))))))

(defun static-bindings->namevec (bindings)
  (coerce (loop for (static-ptree) in bindings nconc (static-ptree-names static-ptree)) 'vector))

(defun bind-static-ptree (static-ptree value function state)
  (etypecase static-ptree
    (ignore state)
    (null (unless (null value) (error "Too many arguments")) state)
    (static-binder (funcall function static-ptree value state))
    (cons (unless (consp value) (error "Not enough arguments"))
     (let ((new (bind-static-ptree (car static-ptree) (car value) function state)))
       (bind-static-ptree (cdr static-ptree) (cdr value) function new)))))

(defun bind-static-ptree-to-vector (static-ptree value vec index)
  (bind-static-ptree static-ptree value
                     (lambda (symbol val index)
                       (declare (cl:ignore symbol))
                       (setf (svref vec index) val)
                       (1+ index))
                     index))

(defun static-fill-values (bindings vec env)
  (loop with index = 0
        for (static-ptree form) in bindings
        do (setf index (bind-static-ptree-to-vector static-ptree (eval form env) vec index))))

(defenv *static* ()
  (defop  $make-static-key (&optional name) ignore
    (multiple-value-list (make-static-key name)))
  (defapp make-static-fixed-environment (binders values &rest parents) ignore
    (apply #'make-static-fixed-environment binders values parents))
  (defapp static-lookup (key environment) ignore (static-lookup key environment))
  ;; convenience applicative - like static-lookup but uses the dynenv.
  (defapp static-variable (key) dynenv (static-lookup key dynenv))
  (defpred static-key? static-key-p)
  (defpred static-binder? static-binder-p)
  ;; Like $let, but with static binders instead of variables.
  ;; Useful for macros (where you'd bind a gensym in Lisp) despite being kind of impossible to
  ;; write in literal code.
  ;; The compiler will need to know special handling procedures for this.
  (defop  $let-static (bindings &rest body) env
    (let* ((names (static-bindings->namevec bindings))
           (values (make-array (length names)))
           (_ (static-fill-values bindings values env))
           (new-env (make-static-fixed-environment names values env)))
      (declare (cl:ignore _))
      (apply #'$sequence new-env body)))
  ;; ($once-only ((a valf)*) body*)
  ;; First, evaluate body in an environment where A etc. are bound to forms that retrieve the
  ;; values of fresh static keys in the dynamic environment.
  ;; Then wrap that result in a $let-static that binds those keys to the VALFs. Return that.
  ;; TODO: Make this into a macro for efficiency - see below for definition
  ;; But maybe it doesn't matter, since only macroexpanders really need to use $once-only anyway.
  (let (;; KLUDGE
        (static-variable (lookup 'syms::static-variable *defining-environment*))
        ($let-static (lookup 'syms::$let-static *defining-environment*)))
    (defop  $once-only (bindings &rest body) env
      (let* (;; analogous to the gensym list.
             (statics
               (mapcar (lambda (bind) (multiple-value-list (make-static-key (first bind))))
                       bindings))
             (forms (mapcar (lambda (static) (list static-variable (second static)))
                            statics))
             (new-env (make-fixed-environment (mapcar #'first bindings) forms env))
             (result (apply #'$sequence new-env body)))
        `(,$let-static (,@(mapcar (lambda (bind static)
                                    `(,(first static) ,(eval (second bind) env)))
                                  bindings statics))
            ,result)))))

#|
;;; Here is how make-keyed-static-variable would be implemented in terms of these.
($define! make-keyed-static-variable
  ($lambda ()
    ($let (((binder key) (make-static-key)))
      (list ($lambda (value env) (make-static-fixed-environment (list binder) (list value) env))
            (wrap ($vau () dynenv (static-lookup key dynenv)))))))
|#

#|
$once-only is, of course, complicated.
now,
($once-only ((a aform) (b bform)) body)
expands to
($let-static (((#<a-binder-binder> #<a-key-binder>) ($make-static-key a))
              ((#<b-binder-binder> #<b-key-binder>) ($make-static-key b)))
 `($let-static ((,(static-var #<a-binder-key>) ,aform)
                (,(static-var #<b-binder-key>) ,bform))
    ,($let-static ((a (static-var ,#<a-key-key>))
                   (b (static-var ,#<b-key-key>)))
  ,@body)))
Actually it seems more practically convenient if the variables are bound to forms that do the lookup,
i.e. static-var forms. This also hides the details of static keys from the disinterested coder.
|#

#+(or)
(defun $once-only (bindings &rest body)
  (let ((outer-statics
          ;; These are the static keys that the once-only expansion uses to hold the static keys
          ;; it creates. Oh boy. There are two for each - one for the binder and one for the reader.
          (loop repeat (length bindings)
                collect (list (multiple-value-list (make-static-key '#:once-only))
                              (multiple-value-list (make-static-key '#:once-only))))))
    `($let-static (,@(loop for ((bind-bind _1) (var-bind _2)) in outer-statics
                           for (name _3) in bindings
                           collect `((,bind-bind ,var-bind) ($make-static-key ,name))))
       ;; The expansion now has bound the static keys, so start on what it returns.
       `($let-static (,,@(loop for ((_1 bind-var) (_2 _3)) in outer-statics
                               for (_4 form) in bindings
                               collect ``(,(static-var ,bind-var) ,,form)))
          ,($let (,@(loop for ((_1 _2) (_3 var-var)) in outer-statics
                          for (name _) in bindings
                          collect `(,name (list 'static-var (static-var ,var-var)))))
             ,@body)))))
