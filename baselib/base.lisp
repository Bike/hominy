(in-package #:hominy/baselib)

;;; Define a "base" environment. This is the environment Hominy starts in normally.
;;; It includes a bunch of useful stuff, which could be called the "standard library" except that
;;; it is not actually conforming to any standard, and won't be until I write one.

(defparameter *base* (i:make-environment *core* *static* *macro* *number* *continuation*))

(defparameter *basec*
  (cenv:make-cenv (list *corec* *staticc* *macroc* *numberc* *continuationc*) t))
