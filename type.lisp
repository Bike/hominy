(in-package #:burke/type)

;;; Simple type mechanism with total subtypep.
;;; Open world (no disjunction is ever top)
;;; Possible extensions:
;;; * general recursion
;;; * structural subtyping for objects?

(defclass type () ())

(defgeneric typep (constant type)
  (:argument-precedence-order type constant))
(defgeneric subtypep (type1 type2))
(defgeneric conjoin/2 (type1 type2)
  (:method ((t1 type) (t2 type)) nil))
(defgeneric disjoin/2 (type1 type2)
  (:method ((t1 type) (t2 type)) nil))
(defgeneric unparse (type))

(defmethod print-object ((type type) stream)
  (multiple-value-bind (up fail) (unparse type)
    (if fail
        (call-next-method)
        (print-unreadable-object (type stream :type t)
          (write up :stream stream)))))

(defclass junction (type)
  ((%types :initarg :types :reader types :type cl:list)))

(defclass conjunction (junction) ())
(defclass disjunction (junction) ())
(defun conjunction (&rest types)
  (if (and (consp types) (null (rest types)))
      (first types)
      (make-instance 'conjunction :types types)))
(defun disjunction (&rest types)
  (if (and (consp types) (null (rest types)))
      (first types)
      (make-instance 'disjunction :types types)))

(defun top () (conjunction))
(defun bot () (disjunction))
(defgeneric top-p (type)
  (:method ((type type)) nil)
  (:method ((type conjunction)) (null (types type))))
(defgeneric bot-p (type)
  (:method ((type type)) nil)
  (:method ((type disjunction)) (null (types type))))

(macrolet
    ((defjoin (name simp junct)
       `(defun ,name (&rest types)
          ;; If any pairwise junctions are simplifiable, recurse with that.
          ;; Otherwise dump into a junction type.
          (loop for (type1 . rest) on types
                do (loop for type2 in rest
                         for j = (,simp type1 type2)
                         when j
                           do (return-from ,name
                                (apply #',name
                                       (append (substitute j type2 rest
                                                           :count 1)
                                               unsimplified))))
                collect type1 into unsimplified
                finally (return (apply #',junct unsimplified))))))
  (defjoin conjoin conjoin/2 conjunction)
  (defjoin disjoin disjoin/2 disjunction))

(defmethod typep (constant (type conjunction))
  (every (lambda (ty) (typep constant ty)) (types type)))
(defmethod typep (constant (type disjunction))
  (some  (lambda (ty) (typep constant ty)) (types type)))

(defmethod subtypep ((t1 conjunction) (t2 type))
  ;; sketchy algebra here
  (if (null (types t1))
      (and (cl:typep t2 'conjunction) (null (types t2)))
      (some (lambda (ty) (subtypep ty t2)) (types t1))))
(defmethod subtypep ((t1 type) (t2 conjunction))
  (every (lambda (ty) (subtypep t1 ty)) (types t2)))

(defmethod subtypep ((t1 disjunction) (t2 type))
  (every (lambda (ty) (subtypep ty t2)) (types t1)))
(defmethod subtypep ((t1 type) (t2 disjunction))
  ;; sketchy algebra here too
  (if (null (types t2))
      (and (cl:typep t1 'disjunction) (null (types t1)))
      (some (lambda (ty) (subtypep t1 ty)) (types t2))))

(defmethod conjoin/2 ((t1 conjunction) (t2 type))
  (apply #'conjoin t2 (types t1)))
(defmethod conjoin/2 ((t1 type) (t2 conjunction))
  (apply #'conjoin t1 (types t2)))
(defmethod disjoin/2 ((t1 disjunction) (t2 type))
  (apply #'disjoin t2 (types t1)))
(defmethod disjoin/2 ((t1 type) (t2 disjunction))
  (apply #'disjoin t1 (types t2)))

(defmethod unparse ((ty conjunction))
  `(and ,@(mapcar #'unparse (types ty))))
(defmethod unparse ((ty disjunction))
  `(or  ,@(mapcar #'unparse (types ty))))

(defclass member (type)
  (;; FIXME: Should have some non metacircular representation of constants
   ;; probably?
   (%elements :initarg :elements :reader elements :type cl:list)))

(defun member (&rest elements)
  (if elements
      (make-instance 'member :elements elements)
      (bot)))

(defmethod typep (constant (type member))
  (cl:member constant (elements type)))
(defmethod subtypep ((t1 member) (t2 type))
  (every (lambda (e) (typep e t2)) (elements t1)))
(defmethod conjoin/2 ((t1 member) (t2 type))
  (apply #'member (remove-if-not (lambda (x) (typep x t2)) (elements t1))))
(defmethod conjoin/2 ((t1 type) (t2 member))
  (apply #'member (remove-if-not (lambda (x) (typep x t1)) (elements t2))))
(defmethod disjoin/2 ((t1 member) (t2 type))
  (if (every (lambda (x) (typep x t2)) (elements t1))
      t2
      nil))
(defmethod disjoin/2 ((t1 type) (t2 member))
  (if (every (lambda (x) (typep x t1)) (elements t2))
      t1
      nil))

(defmethod unparse ((type member)) `(member ,@(elements type)))

(macrolet ((defsingle (name)
             `(progn
                (defclass ,name (type) ())
                (defun ,name () (make-instance ',name))
                (defmethod subtypep ((t1 ,name) (t2 ,name)) t)
                (defmethod conjoin/2 ((t1 ,name) (t2 ,name)) t1)
                (defmethod disjoin/2 ((t1 ,name) (t2 ,name)) t2)
                (defmethod unparse ((type ,name)) ',name)))
           (defsingles (&rest names)
             `(progn ,@(loop for name in names collect `(defsingle ,name)))))
  (defsingles symbol))

(defclass cons (type)
  ((%car :initarg :car :reader car :type type)
   (%cdr :initarg :cdr :reader cdr :type type)))
(defun cons (&optional (car (top)) (cdr (top)))
  (if (or (bot-p car) (bot-p cdr))
      (bot)
      (make-instance 'cons :car car :cdr cdr)))

(defmethod typep ((object cl:cons) (type cons))
  (and (typep (cl:car object) (car type))
       (typep (cl:cdr object) (cdr type))))
(defmethod typep ((object t) (type cons)) nil)
(defmethod subtypep ((t1 cons) (t2 cons))
  (and (subtypep (car t1) (car t2)) (subtypep (cdr t1) (cdr t2))))
(defmethod conjoin/2 ((t1 cons) (t2 cons))
  (cons (conjoin (car t1) (car t2)) (conjoin (cdr t1) (cdr t2))))
(defmethod unparse ((type cons))
  `(cons ,(unparse (car type)) ,(unparse (cdr type))))

(defclass list (type)
  ((%elemtype :initarg :elemtype :reader elemtype :type type)))
(defun list (element-type) (make-instance 'list :elemtype element-type))

(defmethod typep ((object cl:null) (type list)) t)
(defmethod typep ((object cl:cons) (type list))
  ;; FIXME? circularity
  (and (typep (cl:car object) (elemtype type))
       (typep (cl:cdr object) type)))
(defmethod subtypep ((t1 list) (t2 list))
  (subtypep (elemtype t1) (elemtype t2)))
(defmethod subtypep ((t1 cons) (t2 list)) nil)
;; This is only valid if general recursive types are not allowed.
(defmethod subtypep ((t1 list) (t2 cons)) nil)
(defmethod unparse ((type list)) `(list ,(unparse (elemtype type))))

(defclass applicative (type)
  ((%underlying :initarg :underlying :reader unwrap :type type)))
(defun applicative (underlying)
  (make-instance 'applicative :underlying underlying))

(defmethod subtypep ((t1 applicative) (t2 applicative))
  (subtypep (unwrap t1) (unwrap t2)))

(defclass operative (type) ()) ; TODO
(defun operative () (make-instance 'operative))

(defmethod subtypep ((t1 operative) (t2 operative)) (values t t))

(defclass environment (type)
  (;; alist of (symbol . type).
   ;; This type represents all environments that have at least the given
   ;; bindings to the given types.
   (%bindings :initarg :bindings :reader bindings :type list)
   ;; The parent slot here is not conceptually part of the type, which is
   ;; just the set of bindings. But it is convenient to build up types this
   ;; way.
   ;; Bindings shadow parents.
   (%parent :initarg :parent :reader parent :type (or null environment))))

(defun all-bindings (environment)
  (loop with result = ()
        for env = environment then (parent env)
        while env
        do (loop for pair in (bindings env)
                 for name = (car pair)
                 unless (assoc (car name) result)
                   do (push pair result))
        finally (return result)))

(defmethod subtypep ((t1 environment) (t2 environment))
  ;; We're contravariant here - the set of all environments in which X is
  ;; bound to a FOO is a subset of all environments.
  (let ((ab1 (all-bindings t1)) (ab2 (all-bindings t2)))
    (loop for (name . type) in ab2
          for pair1 = (assoc name ab1)
          always (and pair1 (subtypep type (cl:cdr pair1))))))

(macrolet (;; Define k1 and k2 as disjoint.
           (defdisjoin (k1 k2)
             `(progn (defmethod subtypep ((t1 ,k1) (t2 ,k2)) (values nil t))
                     (defmethod subtypep ((t1 ,k2) (t2 ,k1)) (values nil t))))
           ;; Define k to be disjoint to each of ks.
           (defdisjoin1 (k &rest ks)
             `(progn ,@(loop for kp in ks collect `(defdisjoin ,k ,kp))))
           ;; Define all of the kinds to be pairwise disjoint.
           (defdisjoins (&rest kinds)
             (cond ((cl:null kinds) nil)
                   ((cl:null (rest kinds)) nil)
                   (t `(progn
                         (defdisjoin1 ,(first kinds) ,@(rest kinds))
                         (defdisjoins ,@(rest kinds)))))))
  (defdisjoins environment cons applicative operative symbol)
  (defdisjoin1 list environment applicative operative symbol))

;;;

(defmethod car ((type type)) (top))
(defmethod car ((type conjunction))
  (apply #'conjoin (mapcar #'car (types type))))
(defmethod car ((type disjunction))
  (apply #'disjoin (mapcar #'car (types type))))
(defmethod car ((type member))
  (apply #'member
         (loop for e in (elements type)
               when (cl:consp e)
                 collect (cl:car e))))

(defmethod cdr ((type type)) (top))
(defmethod cdr ((type conjunction))
  (apply #'conjoin (mapcar #'cdr (types type))))
(defmethod cdr ((type disjunction))
  (apply #'disjoin (mapcar #'cdr (types type))))
(defmethod cdr ((type member))
  (apply #'member
         (loop for e in (elements type)
               when (cl:consp e)
                 collect (cl:cdr e))))

(defgeneric lookup (symbol-type environment-type)
  (:argument-precedence-order environment-type symbol-type)
  (:method ((symbol type) (environment type)) (top)))

(defun %lookup (symbol environment)
  (loop for env = environment then (parent env)
        while env
        do (loop for (name . type) in (bindings env)
                 when (eq name symbol)
                   do (return-from %lookup type)))
  (top))

(defmethod lookup ((symbol member) (environment environment))
  (apply #'disjoin (mapcar (lambda (s) (%lookup s environment))
                           (elements symbol))))
