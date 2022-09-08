(in-package #:burke/interpreter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Frames
;;;
;;; Frames represent control stack frames. They are not continuations, but
;;; continuations are made out of them. Every combination receives the parent
;;; frame as an argument (representing the stack frame having a link to the
;;; previous stack pointer/return address/etc). The continuation-related
;;; primitives can then make continuations out of it. This means frames can have
;;; dynamic extent and then the primitives can copy them. (the delimited
;;; continuation primitives might have to copy them, since frames are mutable.)
;;;
;;; Frames are essentially a link to the parent plus whatever information is
;;; needed to return to a point in the continuation. The root continuation
;;; has a parent of NIL to indicate it as such. Frames also have marks, which
;;; can be used to implement dynamic binding and guards, but I haven't done
;;; that yet.
;;;
;;; Interpreter frames kind of suck - making a delimited continuation out of
;;; them is going to be awkward. There needs to be enough information in each
;;; frame to reconstruct a delimited continuation later, so that's a lot of
;;; custom handling for anything we do in CL.
;;;
;;; Continuations are like Racket's, i.e. you have prompts, which are kind of
;;; like CL:CATCH, and you can capture the continuation up to a given prompt to
;;; get a delimited continuation. Originally I thought you'd just have explicit
;;; escape continuations (like CL:BLOCK): this is more primitive in a sense, but
;;; has the disadvantage that you're introducing objects that are only valid
;;; within a given dynamic extent, which is pretty much unique to them, and
;;; also makes them impossible to sensibly serialize.
;;;
;;; Unlike Racket, each prompt has two kinds of tag, the catch tag and the throw
;;; tag. Separating these is analogous to the static key thing - you have a
;;; binder and a reader, and you can choose which to make accessible to whom.
;;;
;;; Not having call/cc is pretty different from Kernel. I haven't yet decided
;;; whether to have Kernel's whole continuation hierarchy business, but I do
;;; like the general explicit design of the guards, and I like that the
;;; primitive procedures are based on operating on a continuation object rather
;;; than the current continuation implicitly (as e.g. dynamic-wind does).

;;; Implementation-wise, rather than the usual Scheme interpreter that relies
;;; on the host being tail-recursive and always invokes the continuation
;;; rather than returning normally, we represent Burke control flow as CL
;;; control flow, i.e. we just return normally and stuff. When we want to
;;; abort, we cl:throw. When we want to extend, which CL of course cannot do,
;;; we use a generic function CONTINUE, which performs all the actions in the
;;; accumulated frames. We also mark in the CL dynamic environment where the
;;; extension ends, at which point continue stops tail calling itself and just
;;; returns normally (this cannot be indicated in the frames themselves, which
;;; need to reflect the Burke frames).

;;; Frames allocated during normal evaluation are on the stack. When the code
;;; constructs a delimited continuation, the stack frames are copied into the
;;; heap. It is expected this will be comparatively rare. A lower level
;;; implementation may make a different tradeoff here. But the frames
;;; probably need to be copied anyway since they're mutable (e.g. in the VM).

;;; abstract
(defstruct frame
  (parent (error "missing arg") :type (or null frame) :read-only t))

;;; Return VALUE to FRAME and resume computation.
(defgeneric continue (frame value))

(defvar *extension-base* nil)

(defmethod continue :around ((frame frame) value)
  (if (eq frame *extension-base*)
      value
      (call-next-method)))

(defmethod continue ((frame null) value)
  (error "Somehow reached the root frame with value ~a" value))

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
(defstruct (catch-frame (:include frame)
                        (:constructor make-catch-frame (parent throw-tag)))
  ;; The throw tag for this catch, which doubles as the CL:CATCH tag
  ;; which the interpreter uses to actually abort.
  (throw-tag (error "missing arg") :type throw-tag :read-only t))

(defmethod continue ((f catch-frame) value)
  (continue (frame-parent f) value))

;;; Check if a tag is present on the stack.
(defun tag-available-p (throw-tag pframe)
  (loop for frame = pframe then (frame-parent frame)
        until (null frame)
        when (and (typep frame 'catch-frame)
                  (eq throw-tag (catch-frame-throw-tag frame)))
          return t
        finally (return nil)))

;;; Abort control to the most recent catch with the tag. Signal an error
;;; if the tag is not active.
(defun throw (throw-tag value frame)
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
