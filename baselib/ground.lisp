(in-package #:hominy/baselib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel defines some operatives as derived, but which are really dang
;;; involved that way, like $sequence. Or having to derive them considerably
;;; confuses other definitions, as with $let.

(declaim (inline make-seq-frame))
(defstruct (seq-frame (:include i:frame)
                      (:constructor make-seq-frame (parent env to-go)))
  env
  ;; The list of forms still needing evaluation.
  to-go)

(defun $sequence (env frame &rest forms)
  (cond ((null forms) i:inert)
        ((null (rest forms)) (i:eval (first forms) env frame))
        (t (loop for (form . rest) on forms
                 if (null rest)
                   return (i:eval form env frame) ; tail context
                 else
                   do (let ((frame (make-seq-frame frame env rest)))
                        (declare (dynamic-extent frame))
                        (i:eval form env frame))))))
(defmethod i:continue ((frame seq-frame) value)
  (i:continue (i:frame-parent frame)
              (apply #'$sequence (seq-frame-env frame) (i:frame-parent frame)
                     (seq-frame-to-go frame))))


;;; Returns a function that, given a combinand passed
;;; to an operative, returns a new augmentation of static-env with everything
;;; in the ptree and eparam bound. It sort of pre "compiles" a ptree.
(defun make-augmenter (static-env ptree eparam)
  (etypecase eparam
    (i:ignore
     (multiple-value-bind (names augmenter) (i:ptree-augmenter ptree 0)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce names 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (declare (cl:ignore dynamic-env))
           (let ((vvec (make-array nnames)))
             (funcall augmenter combinand vvec)
             (i:make-fixed-environment names-vec vvec static-env))))))
    (symbol
     (multiple-value-bind (names augmenter) (i:ptree-augmenter ptree 1)
       (declare (type (function (t simple-vector)) augmenter))
       (let* ((names-vec (coerce (list* eparam names) 'vector))
              (nnames (length names-vec)))
         (lambda (dynamic-env combinand)
           (let ((vvec (make-array nnames)))
             (setf (svref vvec 0) dynamic-env)
             (funcall augmenter combinand vvec)
             (i:make-fixed-environment names-vec vvec static-env))))))))

;;; We actually account for circularity here for old time's sake.
;;; And because it's consing anyway, so who cares about consing up a table too?
;;; Besides exposing this function to Hominy, we also use it to copy ptrees and $vau bodies
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

(defclass derived-operative (i:operative)
  ((%ptree :initarg :ptree :reader ptree) 
   (%eparam :initarg :eparam :reader eparam :type (or ignore symbol))
   (%env :initarg :env :reader static-environment)
   ;; A function that, given the dynamic environment and combinand, returns a
   ;; new environment to evaluate the body in. PLIST, EPARAM, and ENV are only
   ;; there for introspection/completeness/whatever. One use is that they can
   ;; be used when deserializing to recompute an augmenter.
   (%augmenter :initarg :augmenter :accessor augmenter :type function)
   ;; A list of forms (not just one form)
   (%body :initarg :body :reader body)))
(defmethod i:name ((object derived-operative))
  `(syms::$vau ,(ptree object) ,(eparam object)))

(defmethod i:combine ((combiner derived-operative) combinand env &optional frame)
  (apply #'$sequence
         (funcall (augmenter combiner) env combinand)
         frame
         (body combiner)))

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
(defstruct (if-frame (:include i:frame)
                     (:constructor make-if-frame (parent then else env)))
  then else env)
(defun bif (value then else dynenv frame)
  (cond ((eq value i:true) (i:eval then dynenv frame))
        ((eq value i:false) (i:eval else dynenv frame))
        (t (error 'type-error :datum value :expected-type 'i:boolean))))
(defmethod i:continue ((frame if-frame) value)
  (i:continue
   (i:frame-parent frame)
   (bif value (if-frame-then frame) (if-frame-else frame) (if-frame-env frame)
        (i:frame-parent frame))))

(declaim (inline make-$define!-frame))
(defstruct ($define!-frame (:include i:frame)
                           (:constructor make-$define!-frame
                               (parent ptree env)))
  ptree env)
(defun b$define! (ptree value env)
  (i:bind-ptree ptree value
                (lambda (symbol val state)
                  (declare (cl:ignore state))
                  (i:define val symbol env))
                nil))
(defmethod i:continue ((frame $define!-frame) value)
  (b$define! ($define!-frame-ptree frame) value ($define!-frame-env frame))
  (i:continue (i:frame-parent frame) i:inert))

(declaim (inline make-$set!-frame))
(defstruct ($set!-frame (:include i:frame)
                        (:constructor make-$set!-frame
                            (parent ptree env)))
  ptree env)
(defun b$set! (ptree value env)
  (i:bind-ptree ptree value
                (lambda (symbol value state)
                  (declare (cl:ignore state))
                  (setf (i:lookup symbol env) value))
                nil))
(defmethod i:continue ((frame $set!-frame) value)
  (b$set! ($set!-frame-ptree frame) value ($set!-frame-env frame))
  (i:continue (i:frame-parent frame) i:inert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defenv (*ground* *groundc*) ()
  ;; core semantics
  (defapp eval (form env) ignore frame (i:eval form env frame))
  (defapp combine (combiner combinand env) ignore frame
    (i:combine combiner combinand env frame))
  (defapp lookup (symbol env) ignore ignore (i:lookup symbol env))
  ;; ignores
  (defpred ignore? (lambda (obj) (eq obj i:ignore)))
  ;; environments
  (defpred environment? (lambda (obj) (typep obj 'i:environment)))
  (defapp make-environment (&rest parents) ignore ignore
    (apply #'i:make-environment parents))
  (defapp make-fixed-environment (symbols values &rest parents) ignore ignore
    (apply #'i:make-fixed-environment symbols values parents))
  (defapp make-immutable-environment (symbols values &rest parents) ignore ignore
    (apply #'i:make-immutable-environment symbols values parents))
  (defapp copy-env-immutable (env) ignore ignore (i:copy-env-immutable env))
  (defop  $define! (ptree form) env frame
    (let ((frame (make-$define!-frame frame ptree env)))
      (declare (dynamic-extent frame))
      (b$define! ptree (i:eval form env frame) env)))
  (defop  $set! (ptree form) env frame
    (let ((frame (make-$set!-frame frame ptree env)))
      (declare (dynamic-extent frame))
      (b$set! ptree (i:eval form env frame) env)))
  ;; operatives
  (defop  $vau (ptree eparam &rest body) static ignore
    (make-derived-operative static ptree eparam body))
  (defpred operative? (lambda (obj) (typep obj 'i:operative)))
  ;; applicatives
  (defpred applicative? (lambda (obj) (typep obj 'i:applicative)))
  (defapp wrap (combiner) ignore ignore (i:wrap combiner))
  (defapp unwrap (applicative) ignore ignore (i:unwrap applicative))
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
  (defapp set-car! (pair object) ignore ignore (rplaca pair object) i:inert)
  (defapp set-cdr! (pair object) ignore ignore (rplacd pair object) i:inert)
  ;; symbols
  (defpred symbol? symbolp)
  ;; objects
  (defapp make-class (n) ignore ignore (make-instance 'i:user-class :nslots n))
  (defapp construct (class &rest args) ignore ignore
    (apply #'i:construct-user-object class args))
  (defapp of-class? (object class) ignore ignore
    (boolify (i:of-user-class-p object class)))
  ;; the idea behind requiring classes here is to provide encapsulation.
  ;; not sure this is the best design though
  (defapp slot-read (class object n) ignore ignore
    (assert (i:of-user-class-p object class))
    (i:slot-access object n))
  (defapp slot-write! (class object n value) ignore ignore
    (assert (i:of-user-class-p object class))
    (setf (i:slot-access object n) value)
    i:inert)
  ;; equivalence
  (defapp eq? (object1 object2) ignore ignore (boolify (eql object1 object2)))
  ;; booleans
  (defop  $if (condition then else) dynenv frame
    (bif (let ((frame (make-if-frame frame then else dynenv)))
           (declare (dynamic-extent frame))
           (i:eval condition dynenv frame))
         then else dynenv frame))
  (defpred boolean? (lambda (obj) (typep obj 'i:boolean)))
  ;; control
  (defop  $sequence (&rest forms) dynenv frame
    (apply #'$sequence dynenv frame forms)))
