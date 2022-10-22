(in-package #:hominy/treec)

;;; Handling for when the compiler needs to evaluate something, e.g. to resolve
;;; constants or to expand macros.

;;; Although not handled yet, the compiler probably should establish a
;;; continuation barrier to prevent later reentry into to the compiler.
;;; But aborting out of the compiler should be ok.

;;; Also not implemented are operators that allow the programmer to bind new
;;; stuff in the evaluation environment.

(defvar *evaluation-frame*)
(defvar *evaluation-env*)

(defun cteval (form) (i:eval form *evaluation-env* *evaluation-frame*))
(defun ctcombine (combiner combinand)
  (i:combine combiner combinand *evaluation-env* *evaluation-frame*))
