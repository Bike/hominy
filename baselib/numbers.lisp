(in-package #:burke/baselib)

;;; Kernel has a couple impedance mismatches with CL here.
;;; In particular I have not really looked at "inexact" numbers much.
;;; So for now only rationals are supported.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass exact-infinity () ())
  (defclass -infinity (exact-infinity) ())
  (defclass +infinity (exact-infinity) ()))
(defconstant -infinity
  (if (boundp '-infinity) (symbol-value '-infinity) (make-instance '-infinity)))
(defconstant +infinity
  (if (boundp '+infinity) (symbol-value '+infinity) (make-instance '+infinity)))
(defmethod make-load-form ((object exact-infinity) &optional env)
  (make-load-form-saving-slots object :environment env))
(defmethod print-object ((object -infinity) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "-infinity" stream)))
(defmethod print-object ((object +infinity) stream)
  (if *print-escape*
      (call-next-method)
      (write-string "+infinity" stream)))

(defun =?/2 (n1 n2)
  (boolify
   (etypecase n1
     (rational (etypecase n2
                 (rational (= n1 n2))
                 (exact-infinity nil)))
     (exact-infinity (etypecase n2
                       (rational nil)
                       (exact-infinity (eql n1 n2)))))))

(defun <?/2 (n1 n2)
  (boolify
   (etypecase n1
     (rational (etypecase n2
                 (rational (< n1 n2))
                 (-infinity nil)
                 (+infinity t)))
     (-infinity (etypecase n2
                  (-infinity nil)
                  ((or rational +infinity) t)))
     (+infinity (etypecase n2
                  ((or rational -infinity +infinity) nil))))))

(defun <=?/2 (n1 n2)
  (boolify
   (etypecase n1
     (rational (etypecase n2
                 (rational (<= n1 n2))
                 (-infinity nil)
                 (+infinity t)))
     (-infinity (etypecase n2
                  ((or rational -infinity +infinity) t)))
     (+infinity (etypecase n2
                  (+infinity t)
                  ((or rational -infinity) nil))))))

(defun >?/2 (n1 n2) (<?/2 n2 n1))
(defun >=?/2 (n1 n2) (<=?/2 n2 n1))

(defun +/2 (n1 n2)
  (etypecase n1
    (rational (etypecase n2
                (rational (+ n1 n2))
                (exact-infinity n2)))
    ;; sum of -infinity and +infinity has no primary value, which is an error with strict
    ;; arithmetic. which i'm just gonna say is on.
    (-infinity (etypecase n2
                 ((or rational -infinity) n1)))
    (+infinity (etypecase n2
                 ((or rational +infinity) n1)))))

(defun */2 (n1 n2)
  (etypecase n1
    ((eql 0) (etypecase n2
               (rational (* n1 n2))))
    (rational (etypecase n2
                (rational (* n1 n2))
                (-infinity (if (< n1 0) +infinity n2))
                (+infinity (if (< n1 0) -infinity n2))))
    (-infinity (etypecase n2
                 ((rational * (0)) +infinity)
                 ((rational (0)) n1)
                 (-infinity +infinity)
                 (+infinity n1)))
    (+infinity (etypecase n2
                 ((rational * (0)) -infinity)
                 ((rational (0)) n1)
                 (+infinity -infinity)
                 (-infinity n1)))))

(defun -/2 (n1 n2)
  (etypecase n1
    (rational (etypecase n2
                (rational (- n1 n2))
                (-infinity +infinity)
                (+infinity -infinity)))
    (-infinity (etypecase n2
                 ((or rational +infinity) n1)))
    (+infinity (etypecase n2
                 ((or rational -infinity) n1)))))

(defun //2 (n1 n2)
  (etypecase n1
    (rational (etypecase n2
                (rational (/ n1 n2))
                (exact-infinity 0)))
    (exact-infinity (etypecase n2 (rational n1)))))

(defenv (*number* *numberc*) ()
  ;; KLUDGE: In Kernel these are actual number syntax, not constants,
  ;; meaning the reader should be returning infinities when it sees these, not a symbol.
  (i:define -infinity 'syms::-infinity *defining-environment*)
  (i:define +infinity 'syms::+infinity *defining-environment*)
  ;; TODO: Extend a bunch of this stuff for any arity.
  (defapp number? (object) ignore ignore
    (boolify (typep object '(or rational exact-infinity))))
  (defapp finite? (object) ignore ignore
    (etypecase object
      (rational i:true)
      (exact-infinity i:false)))
  (defapp integer? (object) ignore ignore (boolify (integerp object)))
  (defapp =? (num1 num2) ignore ignore (=?/2 num1 num2))
  (defapp <? (num1 num2) ignore ignore (<?/2 num1 num2))
  (defapp <=? (num1 num2) ignore ignore (<=?/2 num1 num2))
  (defapp >? (num1 num2) ignore ignore (>?/2 num1 num2))
  (defapp >=? (num1 num2) ignore ignore (>=?/2 num1 num2))
  (defapp + (n1 n2) ignore ignore (+/2 n1 n2))
  (defapp * (n1 n2) ignore ignore (*/2 n1 n2))
  (defapp - (n1 n2) ignore ignore (-/2 n1 n2))
  (defapp / (n1 n2) ignore ignore (//2 n1 n2))
  (defapp zero? (number) ignore ignore
    (boolify (etypecase number (rational (zerop number)) (exact-infinity nil))))
  (defapp positive? (number) ignore ignore
    (etypecase number
      ((rational * (0)) i:false)
      ((rational (0)) i:true)
      (-infinity i:false)
      (+infinity i:true)))
  (defapp negative? (number) ignore ignore
    (etypecase number
      ((rational * (0)) i:true)
      ((rational (0)) i:false)
      (-infinity i:true)
      (+infinity i:false)))
  (defapp odd? (number) ignore ignore
    (boolify (etypecase number (integer (oddp number)))))
  (defapp even? (number) ignore ignore
    (boolify (etypecase number (integer (evenp number)))))
  (defapp rational? (object) ignore ignore (boolify (rationalp object)))
  (defapp numerator (rational) ignore ignore (numerator rational))
  (defapp denominator (rational) ignore ignore (denominator rational))
  (defapp floor (real) ignore ignore
    (etypecase real
      (rational (floor real))
      (exact-infinity real)))
  (defapp ceiling (real) ignore ignore
    (etypecase real
      (rational (ceiling real))
      (exact-infinity real)))
  (defapp truncate (real) ignore ignore
    (etypecase real
      (rational (truncate real))
      (exact-infinity real)))
  (defapp round (real) ignore ignore
    (etypecase real
      (rational (round real))
      (exact-infinity real))))
