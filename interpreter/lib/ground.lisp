(in-package #:burke/interpreter)

(defun bindings->namesvec (bindings)
  (coerce (loop for (ptree) in bindings nconc (ptree-names ptree)) 'vector))

(defun fill-values (bindings vec env frame)
  (loop with start = 0
        for (ptree form) in bindings
        for value = (eval form env frame)
        do (setf start (bind-ptree-to-vector ptree value vec start))))

;;; Returns a function that, given a combinand passed
;;; to an operative, returns a new augmentation of static-env with everything
;;; in the ptree and eparam bound. It sort of pre "compiles" a ptree.
(defun make-augmenter (static-env ptree eparam)
  (etypecase eparam
    (ignore
     (multiple-value-bind (names augmenter) (ptree-augmenter ptree 0)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce names 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (declare (cl:ignore dynamic-env))
           (let ((vvec (make-array nnames)))
             (funcall augmenter combinand vvec)
             (make-fixed-environment names-vec vvec static-env))))))
    (symbol
     (multiple-value-bind (names augmenter) (ptree-augmenter ptree 1)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce (list* eparam names) 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (let ((vvec (make-array nnames)))
             (setf (svref vvec 0) dynamic-env)
             (funcall augmenter combinand vvec)
             (make-fixed-environment names-vec vvec static-env))))))))

;;; We actually account for circularity here for old time's sake.
;;; And because it's consing anyway, so who cares about consing up a table too?
;;; Besides exposing this function to Burke, we also use it to copy ptrees and $vau bodies
;;; and stuff, like Kernel does. Prevents programmers from doing goofy bullshit.
;;; In Kernel the copy's immutable, which would also be good for that (and does matter
;;; because the programmer could in some situations get at the substructures) but we
;;; don't have immutable conses. I guess we should, maybe?
(defun copy-es (object)
  (if (consp object)
      (let ((table (make-hash-table :test #'eq)))
        (labels ((copy (object)
                   (cond ((atom object) object)
                         ((gethash object table))
                         (t
                          (let ((copy (cons nil nil)))
                            (setf (gethash object table) copy)
                            (setf (car copy) (copy (car object))
                                  (cdr copy) (copy (cdr object)))
                            copy)))))
          (copy object)))
      object))

(defun make-derived-operative (static-env ptree eparam body)
  (let ((aug (make-augmenter static-env ptree eparam)))
    (make-instance 'derived-operative
      :ptree (copy-es ptree) :eparam eparam :env static-env :augmenter aug
      ;; Used to do (cons '$sequence body) here, but then $sequence becoming
      ;; rebound would be an issue, so instead the COMBINE method has been
      ;; modified to do a sequence of forms directly.
      :body (copy-es body))))

;;; Frame for evaluating an $if condition.
(declaim (inline make-if-frame))
(defstruct (if-frame (:include frame)
                     (:constructor make-if-frame (parent then else env)))
  then else env)
(defun bif (value then else dynenv frame)
  (cond ((eq value true) (eval then dynenv frame))
        ((eq value false) (eval else dynenv frame))
        (t (error 'type-error :datum value :expected-type 'boolean))))
(defmethod continue ((frame if-frame) value)
  (continue
   (frame-parent frame)
   (bif value (if-frame-then frame) (if-frame-else frame) (if-frame-env frame)
        (frame-parent frame))))

(declaim (inline make-$define!-frame))
(defstruct ($define!-frame (:include frame)
                           (:constructor make-$define!-frame
                               (parent ptree env)))
  ptree env)
(defun b$define! (ptree value env)
  (bind-ptree ptree value
              (lambda (symbol val state)
                (declare (cl:ignore state))
                (define val symbol env))
              nil))
(defmethod continue ((frame $define!-frame) value)
  (b$define! ($define!-frame-ptree frame) value ($define!-frame-env frame))
  (continue (frame-parent frame) inert))

(declaim (inline make-$set!-frame))
(defstruct ($set!-frame (:include frame)
                        (:constructor make-$set!-frame
                            (parent ptree env)))
  ptree env)
(defun b$set! (ptree value env)
  (bind-ptree ptree value
              (lambda (symbol value state)
                (declare (cl:ignore state))
                (setf (lookup symbol env) value))
              nil))
(defmethod continue ((frame $set!-frame) value)
  (b$set! ($set!-frame-ptree frame) value ($set!-frame-env frame))
  (continue (frame-parent frame) inert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defenv *ground* ()
  ;; core semantics
  (defapp eval (form env) ignore frame (eval form env frame))
  (defapp combine (combiner combinand env) ignore frame
    (combine combiner combinand env frame))
  (defapp lookup (symbol env) ignore ignore (lookup symbol env))
  ;; ignores
  (defpred ignore? ignorep)
  ;; environments
  (defpred environment? environmentp)
  (defapp make-environment (&rest parents) ignore ignore
    (apply #'make-environment parents))
  (defapp make-fixed-environment (symbols values &rest parents) ignore ignore
    (apply #'make-fixed-environment symbols values parents))
  (defapp make-immutable-environment (symbols values &rest parents) ignore ignore
    (apply #'make-immutable-environment symbols values parents))
  (defapp copy-env-immutable (env) ignore ignore (copy-env-immutable env))
  (defop  $define! (ptree form) env frame
    (let ((frame (make-$define!-frame frame ptree env)))
      (declare (dynamic-extent frame))
      (b$define! ptree (eval form env frame) env)))
  (defop  $set! (ptree form) env frame
    (let ((frame (make-$set!-frame frame ptree env)))
      (declare (dynamic-extent frame))
      (b$set! ptree (eval form env frame) env)))
  ;; operatives
  (defop  $vau (ptree eparam &rest body) static ignore
    (make-derived-operative static ptree eparam body))
  (defpred operative? operativep)
  ;; applicatives
  (defpred applicative? applicativep)
  (defapp wrap (combiner) ignore ignore (wrap combiner))
  (defapp unwrap (applicative) ignore ignore (unwrap applicative))
  ;; lists
  (defapp cons (car cdr) ignore ignore (cons car cdr))
  (defapp car (cons) ignore ignore
    (if (typep cons 'cons)
        (car (the cons cons))
        (error 'type-error :datum cons :expected-type 'cons)))
  (defapp cdr (cons) ignore ignore
    (if (typep cons 'cons)
        (cdr (the cons cons))
        (error 'type-error :datum cons :expected-type 'cons)))
  (defpred cons? consp)
  (defpred null? null)
  (defapp copy-es (object) ignore ignore (copy-es object))
  (defapp set-car! (pair object) ignore ignore (rplaca pair object) inert)
  (defapp set-cdr! (pair object) ignore ignore (rplacd pair object) inert)
  ;; symbols
  (defpred symbol? symbolp)
  ;; equivalence
  (defapp eq? (object1 object2) ignore ignore (boolify (eql object1 object2)))
  ;; booleans
  (defop  $if (condition then else) dynenv frame
    (bif (let ((frame (make-if-frame frame then else dynenv)))
           (declare (dynamic-extent frame))
           (eval condition dynenv frame))
         then else dynenv frame))
  (defpred boolean? booleanp)
  ;; control
  (defop  $sequence (&rest forms) dynenv frame
    (apply #'$sequence dynenv frame forms))
  (defop  $make-catch-tag (name) ignore ignore
    (multiple-value-list (make-catch-tag name)))
  (defop  $catch (tag &rest body) dynenv frame
    ;; FIXME: Frame for evaluating a catch tag.
    (fcatch (eval tag dynenv frame)
            (lambda (frame) (apply #'$sequence dynenv frame body))
            frame))
  (defapp throw (tag value) ignore frame (throw tag value frame))
  ;; FIXME: Should be relativized to a continuation argument.
  ;; In Racket, this (continuation-prompt-available?) has an optional
  ;; continuation parameter, which is the current continuation
  ;; by default.
  (defapp tag-available? (tag) ignore frame
    (continue frame (boolify (tag-available-p tag frame))))
  ;; environment stuff
  (defop  $let (bindings &rest body) env frame
    ;; FIXME: Frames for the value evaluations.
    ;; (Alternately, skip that by just macroexpanding to an application?)
    (let* ((names (bindings->namesvec bindings))
           (values (make-array (length names)))
           (_ (fill-values bindings values env frame))
           (new-env (make-fixed-environment names values env)))
      (declare (cl:ignore _))
      (apply #'$sequence new-env frame body)))
  (defapp exit (&rest values) ignore ignore (cl:throw 'abort values)))
