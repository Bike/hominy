(in-package #:burke/type)

;;; Simple type mechanism with total subtypep.
;;; Open world (no disjunction is ever top)
;;; Possible extensions:
;;; * general recursion
;;; * structural subtyping for environments (and objects?)

(defclass type () ())

(defgeneric subtypep (type1 type2))
(defgeneric conjoin/2 (type1 type2))
(defgeneric disjoin/2 (type1 type2))

(defclass top (type) ())
(defun top () (make-instance 'top))
(defclass bot (type) ())
(defun bot () (make-instance 'bot))

(defun conjoin (&rest types)
  (cond ((null types) (top))
        ((null (rest types)) (first types))
        (t (reduce #'conjoin/2 types))))
(define-compiler-macro conjoin (&rest types)
  (cond ((null types) `(top))
        ((null (rest types)) `(the type ,(first types))) ; avoid toplevel
        (t `(conjoin/2 ,(first types) (conjoin ,@(rest types))))))
(defun disjoin (&rest types)
  (cond ((null types) (bot))
        ((null (rest types)) (first types))
        (t (reduce #'disjoin/2 types))))
(define-compiler-macro disjoin (&rest types)
  (cond ((null types) `(bot))
        ((null (rest types)) `(the type ,(first types)))
        (t `(disjoin/2 ,(first types) (disjoin ,@(rest types))))))

(defmethod subtypep ((t1 type) (t2 top)) t)
(defmethod subtypep ((t1 top) (t2 top)) t)
(defmethod subtypep ((t1 top) (t2 type)) nil)
(defmethod subtypep ((t1 bot) (t2 type)) t)
(defmethod subtypep ((t1 bot) (t2 bot)) t)
(defmethod subtypep ((t1 type) (t2 bot)) nil)

(defmethod conjoin/2 ((t1 top) (t2 type)) t2)
(defmethod conjoin/2 ((t1 type) (t2 top)) t1)
(defmethod conjoin/2 ((t1 bot) (t2 type)) t1)
(defmethod conjoin/2 ((t1 type) (t2 bot)) t2)
(defmethod disjoin/2 ((t1 top) (t2 type)) t1)
(defmethod disjoin/2 ((t1 type) (t2 top)) t2)
(defmethod disjoin/2 ((t1 bot) (t2 type)) t2)
(defmethod disjoin/2 ((t1 type) (t2 bot)) t1)

(macrolet ((defsingle (name)
             `(progn
                (defclass ,name (type) ())
                (defun ,name () (make-instance ',name))
                (defmethod subtypep ((t1 ,name) (t2 ,name)) t)
                (defmethod conjoin/2 ((t1 ,name) (t2 ,name)) t1)
                (defmethod disjoin/2 ((t1 ,name) (t2 ,name)) t2)))
           (defsingles (&rest names)
             `(progn ,@(loop for name in names collect `(defsingle ,name)))))
  (defsingles environment null inert ignore symbol))

(defclass cons (type)
  ((%car :initarg :car :reader car :type type)
   (%cdr :initarg :cdr :reader cdr :type type)))
(defun cons (&optional (car (top)) (cdr (top)))
  (make-instance 'cons :car car :cdr cdr))

(defmethod subtypep ((t1 cons) (t2 cons))
  (and (subtypep (car t1) (car t2)) (subtypep (cdr t1) (cdr t2))))

(defclass list (type)
  ((%elemtype :initarg :elemtype :reader elemtype :type type)))
(defun list (element-type) (make-instance 'list :elemtype element-type))

(defmethod subtypep ((t1 list) (t2 list))
  (subtypep (elemtype t1) (elemtype t2)))
(defmethod subtypep ((t1 null) (t2 list)) t)
(defmethod subtypep ((t1 list) (t2 null)) nil)
(defmethod subtypep ((t1 cons) (t2 list))
  (and (subtypep (car t1) (elemtype t2)) (subtypep (cdr t1) t2)))
;; This is only valid if general recursive types are not allowed.
(defmethod subtypep ((t1 list) (t2 cons)) nil)

(defclass applicative (type)
  ((%underlying :initarg :underlying :reader unwrap :type type)))
(defun applicative (underlying)
  (make-instance 'applicative :underlying underlying))

(defmethod subtypep ((t1 applicative) (t2 applicative))
  (subtypep (unwrap t1) (unwrap t2)))

(defclass operative (type) ()) ; TODO
(defun operative () (make-instance 'operative))

(defmethod subtypep ((t1 operative) (t2 operative)) (values t t))

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
  (defdisjoins environment inert ignore null cons applicative operative symbol)
  (defdisjoin1 list environment inert ignore applicative operative symbol))
