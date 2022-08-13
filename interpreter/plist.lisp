(in-package #:burke/interpreter)

;;;; Operators for dealing with plists. Plists, or "parameter lists", are how
;;;; Burke binds things. For example, the parameters to a function, or the
;;;; bindings in a $let, are plists.
;;;; Burke plists are mostly like Kernel plists, except that they must not be
;;;; circular/self referential.
;;;; A plist can be either ignore, (), a symbol, or a cons of plists.
;;;; IGNORE binds nothing. The value is ignored.
;;;; () binds nothing. If the value is not also (), an error is signaled.
;;;; A symbol binds to the value.
;;;; (plista . plistb) bound to (valuea . valueb) binds plista to valuea and
;;;;  plistb to valueb. If the value is not a cons, an error is signaled.

;;; Given a plist, return an ordered list of names in the plist.
(defun plist-names (plist)
  (etypecase plist
    ((or ignore null) nil)
    (symbol (list plist))
    (cons (nconc (plist-names (car plist)) (plist-names (cdr plist))))))

;;; Maps through the plist and a value.
;;; Calls FUNCTION on each symbol in order, along with the value for it,
;;; and the state. The state starts as INITIAL, and is then set to whatever
;;; the function returns.
(defun bind-plist (plist value function initial)
  (etypecase plist
    (ignore initial)
    (null (unless (null value) (error "Too many arguments")) initial)
    (symbol (funcall function plist value initial))
    (cons (unless (consp value) (error "Not enough arguments"))
     (let ((new (bind-plist (car plist) (car value) function initial)))
       (bind-plist (cdr plist) (cdr value) function new)))))

(defun bind-plist-to-vector (plist value vec start)
  (bind-plist plist value
              (lambda (symbol val start)
                (declare (cl:ignore symbol))
                (setf (svref vec start) val)
                (1+ start))
              start))

;;; Given a plist and an array index, returns two values. The first is an
;;; ordered list of names in the plist. The second is a function of two
;;; arguments, a combinand and a vector. This function will deconstruct the
;;; combinand according to the plist, and store values into the given vector,
;;; starting at the index. The values' positions will correspond to those of
;;; the names.
;;; This is sort of a "precompiled" version of bind-plist-to-vector.
(defun plist-augmenter (plist start)
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
              (plist-augmenter (car plist) start)
            (multiple-value-bind (cdr-names cdr-augmenter)
                (plist-augmenter (cdr plist) (+ start (length car-names)))
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
