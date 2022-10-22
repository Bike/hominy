(in-package #:hominy/vm)

(defgeneric construct (class &rest arguments))
(defgeneric of-class-p (object class)
  (:argument-precedence-order class object))
(defgeneric slot-access (class object index))
(defgeneric (setf slot-access) (new class object index)
  (:argument-precedence-order class object index new))

(defclass proxy-class (class)
  ((%name :initarg :name :reader i:name :type symbol) ; debugging
   (%constructor :initarg :constructor :reader constructor :type function)
   (%nslots :initarg :nslots :reader nslots :type (integer 0))
   (%tester :initarg :tester :reader tester :type function)
   (%reader :initarg :reader :reader reader :type function)
   (%writer :initarg :writer :reader writer :type function)))

(defmethod print-object ((o proxy-class) s)
  (print-unreadable-object (o s :type t)
    (when (slot-boundp o '%name)
      (write (i:name o) :stream s)))
  o)

(defun make-proxy (name constructor nargs tester reader writer)
  (make-instance 'proxy-class
    :name name :constructor constructor :nslots nargs
    :tester tester :reader reader :writer writer))

(defmethod construct ((class proxy-class) &rest arguments)
  (apply (constructor class) arguments))
(defmethod of-class-p (object (class proxy-class))
  (funcall (tester class) object))
(defmethod slot-access ((class proxy-class) object index)
  (funcall (reader class) object index))
(defmethod (setf slot-access) (new (class proxy-class) object index)
  (funcall (writer class) new object index)
  new)


;;; some builtin classes. right now this is just ones the VM wants.

(defun illegal-construction (&rest args)
  (declare (ignore args))
  (error "Can't make an instance of this class"))
(defun illegal-read (object index)
  (declare (ignore index))
  (error "Can't read a slot of this object: ~s" object))
(defun illegal-write (new object index)
  (declare (ignore new index))
  (error "Can't write a slot of this object: ~s" object))

(defparameter class:cell (make-proxy 'class:cell #'i:make-cell 1 #'i:cellp
                                     (lambda (cell index)
                                       (declare (ignore index))
                                       (i:cell-value cell))
                                     (lambda (new cell index)
                                       (declare (ignore index))
                                       (setf (i:cell-value cell) new))))
(defparameter class:cons (make-proxy 'class:cons #'cons 2 #'consp
                                     (lambda (cons index)
                                       (ecase index
                                         ((0) (car cons))
                                         ((1) (cdr cons))))
                                     (lambda (new cons index)
                                       (ecase index
                                         ((0) (setf (car cons) new))
                                         ((1) (setf (cdr cons) new))))))
(defparameter class:boolean (make-proxy 'class:boolean #'illegal-construction 0
                                        (lambda (o) (typep o 'boolean))
                                        #'illegal-read #'illegal-write))
(defparameter class:null (make-proxy 'class:null #'illegal-construction 0 #'null
                                     #'illegal-read #'illegal-write))
(defparameter class:applicative
  (make-proxy 'class:applicative #'i:wrap 1 (lambda (o) (typep o 'i:applicative))
              (lambda (app index) (declare (ignore index)) (i:unwrap app))
              #'illegal-write))

(defmethod nslots ((class i:user-class)) (i:nslots class))
(defmethod construct ((class i:user-class) &rest args)
  (apply #'i:construct-user-object class args))
(defmethod of-class-p (object (class i:user-class))
  (i:of-user-class-p object class))
(defmethod slot-access ((class i:user-class) object index)
  (i:slot-access object index))
(defmethod (setf slot-access) (new (class i:user-class) object index)
  (setf (i:slot-access object index) new))
