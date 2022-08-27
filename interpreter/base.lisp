(in-package #:burke/interpreter)

;;; Define a "base" environment. This is the environment Burke starts in normally.
;;; It includes a bunch of useful stuff, which could be called the "standard library" except that
;;; it is not actually conforming to any standard, and won't be until I write one.

(defparameter *base* (make-environment *core* *static* *macro*))
