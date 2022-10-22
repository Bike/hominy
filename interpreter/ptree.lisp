(in-package #:hominy/interpreter)

;;;; Operators for dealing with ptrees. Plists, or "parameter lists", are how
;;;; Hominy binds things. For example, the parameters to a function, or the
;;;; bindings in a $let, are ptrees.
;;;; Hominy ptrees are mostly like Kernel ptrees, except that they must not be
;;;; circular/self referential.
;;;; A ptree can be either ignore, (), a symbol, or a cons of ptrees.
;;;; IGNORE binds nothing. The value is ignored.
;;;; () binds nothing. If the value is not also (), an error is signaled.
;;;; A symbol binds to the value.
;;;; (ptreea . ptreeb) bound to (valuea . valueb) binds ptreea to valuea and
;;;;  ptreeb to valueb. If the value is not a cons, an error is signaled.

;;; Given a ptree, return an ordered list of names in the ptree.
(defun ptree-names (ptree)
  (etypecase ptree
    ((or ignore null) nil)
    (symbol (list ptree))
    (cons (nconc (ptree-names (car ptree)) (ptree-names (cdr ptree))))))

;;; Maps through the ptree and a value.
;;; Calls FUNCTION on each symbol in order, along with the value for it,
;;; and the state. The state starts as INITIAL, and is then set to whatever
;;; the function returns.
(defun bind-ptree (ptree value function initial)
  (etypecase ptree
    (ignore initial)
    (null (unless (null value) (error "Too many arguments")) initial)
    (symbol (funcall function ptree value initial))
    (cons (unless (consp value) (error "Not enough arguments"))
     (let ((new (bind-ptree (car ptree) (car value) function initial)))
       (bind-ptree (cdr ptree) (cdr value) function new)))))

(defun bind-ptree-to-vector (ptree value vec start)
  (bind-ptree ptree value
              (lambda (symbol val start)
                (declare (cl:ignore symbol))
                (setf (svref vec start) val)
                (1+ start))
              start))

;;; Given a ptree and an array index, returns two values. The first is an
;;; ordered list of names in the ptree. The second is a function of two
;;; arguments, a combinand and a vector. This function will deconstruct the
;;; combinand according to the ptree, and store values into the given vector,
;;; starting at the index. The values' positions will correspond to those of
;;; the names.
;;; This is sort of a "precompiled" version of bind-ptree-to-vector.
(defun ptree-augmenter (ptree start)
  (etypecase ptree
    (ignore (values nil (lambda (combinand vec)
                          (declare (cl:ignore combinand vec)
                                   ;; hopefully prevents argcount check
                                   (optimize speed (safety 0))))))
    (null (values nil (lambda (combinand vec)
                        (declare (cl:ignore vec) (optimize speed (safety 0)))
                        (unless (null combinand)
                          (error "Too many arguments")))))
    (symbol (values (list ptree)
                    (lambda (combinand vec)
                      ;; prevent argcount check and vector length check
                      (declare (optimize speed (safety 0)))
                      (setf (svref vec start) combinand))))
    (cons (multiple-value-bind (car-names car-augmenter)
              (ptree-augmenter (car ptree) start)
            (multiple-value-bind (cdr-names cdr-augmenter)
                (ptree-augmenter (cdr ptree) (+ start (length car-names)))
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
