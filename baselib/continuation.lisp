(in-package #:hominy/baselib)

(defclass catch-tag ()
  ((%name :initform nil :initarg :name :reader name)
   (%throw-tag-wp :initarg :throw-tag-wp :reader %throw-tag-wp)))

(defun throw-tag (catch-tag)
  (trivial-garbage:weak-pointer-value (%throw-tag-wp catch-tag)))

(defmethod print-object ((o catch-tag) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (write (name o) :stream stream))
  o)

(defclass throw-tag ()
  ((%name :initform nil :initarg :name :reader name)))

(defmethod print-object ((o throw-tag) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (write (name o) :stream stream)))

(defun make-catch-tag (name)
  (let* ((throw-tag (make-instance 'throw-tag :name name))
         (wp (trivial-garbage:make-weak-pointer throw-tag)))
    (values (make-instance 'catch-tag :name name :throw-tag-wp wp)
            throw-tag)))

;;; A frame for a catch, inserted by call/catch or fcatch
(declaim (inline make-catch-frame))
(defstruct (catch-frame (:include i:frame)
                        (:constructor make-catch-frame (parent throw-tag)))
  ;; The throw tag for this catch, which doubles as the CL:CATCH tag
  ;; which the interpreter uses to actually abort.
  (throw-tag (error "missing arg") :type throw-tag :read-only t))

(defmethod i:continue ((f catch-frame) value)
  (i:continue (i:frame-parent f) value))

;;; Check if a tag is present on the stack.
(defun tag-available-p (throw-tag pframe)
  (loop for frame = pframe then (i:frame-parent frame)
        until (null frame)
        when (and (typep frame 'catch-frame)
                  (eq throw-tag (catch-frame-throw-tag frame)))
          return t
        finally (return nil)))

;;; Abort control to the most recent catch with the tag. Signal an error
;;; if the tag is not active.
(defun fthrow (throw-tag value frame)
  (if (tag-available-p throw-tag frame)
      (cl:throw throw-tag value)
      (error "Tag ~a is not active" throw-tag)))
  
;;; Establish a point throw can abort to, and call FUNCTION with
;;; the new catch-frame and then the rest of the arguments.
;;; If the catch's throw tag is not accessible, a new frame is not actually
;;; established, so callers should be prepared for that.
(defun fcatch (catch-tag function frame &rest args)
  (let ((throw-tag (throw-tag catch-tag)))
    (if throw-tag
        (cl:catch throw-tag
          (let ((frame (make-catch-frame frame throw-tag)))
            (declare (dynamic-extent frame))
            (apply function frame args)))
        (apply function frame args))))

#+(or)
(defun extend (frame dcont value)
  (let ((most-recent-frame (copy-dcont dcont frame))
        (*extension-base* frame))
    (continue most-recent-frame value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defenv (*continuation* *continuationc*) ()
  (defop  $make-catch-tag (name) ignore ignore
    (multiple-value-list (make-catch-tag name)))
  (defop  $catch (tag &rest body) dynenv frame
    ;; FIXME: Frame for evaluating a catch tag.
    (fcatch (i:eval tag dynenv frame)
            (lambda (frame) (apply #'$sequence dynenv frame body))
            frame))
  (defapp throw (tag value) ignore frame (fthrow tag value frame))
  ;; FIXME: Should be relativized to a continuation argument.
  ;; In Racket, this (continuation-prompt-available?) has an optional
  ;; continuation parameter, which is the current continuation
  ;; by default.
  (defapp tag-available? (tag) ignore frame
    (boolify (tag-available-p tag frame)))
  (defapp exit (&rest values) ignore ignore (cl:throw 'abort values)))
