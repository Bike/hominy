(in-package #:burke/ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR modifications
;;;

(defun %replace-use (use new)
  (let ((old (definition use)))
    (setf (%definition use) new)
    (%remove-use old use)
    (unless (null new) (%add-use new use))))

(defun %use-replacer (new) (lambda (use) (%replace-use use new)))
(defun %deuse-inputs (user) (map nil (%use-replacer nil) (%uinputs user)))

;;; Called automatically by %REMOVE-USE
;;; TODO: Warn about unused code etc. maybe?
(defgeneric %cleanup (datum))

(defmethod %cleanup :before ((datum datum)) (assert (null (%uses datum))))

(defmethod %cleanup ((datum parameter)))
(defmethod %cleanup ((datum enclosed)))
(defmethod %cleanup ((datum constant)))

(defmethod %cleanup ((inst bind))
  (%deuse-inputs inst)
  (let ((cont (continuation inst)) (prev (%prev inst)) (next (next inst)))
    (if (null prev) ; first instruction in this continuation
        (setf (%start cont) next)
        (setf (%next prev) next))
    (setf (%prev next) prev)))

(defun %delete-terminator (term) (%deuse-inputs term))

(defmethod %cleanup ((continuation continuation))
  (%remove-child (parent continuation) continuation)
  (%deuse-inputs (parameter continuation))
  ;; Delete backwards to avoid tripping cleanup assertions.
  (let* ((term (terminator continuation)) (inst (%prev term)))
    (%delete-terminator term)
    (loop for prev = (%prev inst)
          do (%cleanup inst) (setf inst prev)
          until (null inst))))

(defmethod %cleanup ((datum function))
  ;; We have to be kind of careful to avoid tripping the unusability assertions.
  ;; So first we delete all terminators, which makes all continuations unused.
  ;; Then we delete continuations in reverse dominance order, i.e. a
  ;; continuation is always deleted after any continuations it dominates, so
  ;; that any temporaries are really definitely unused.
  (map-continuations (lambda (cont) (%delete-terminator (terminator cont)))
                     datum)
  ;; postorder deletion (map-continuations is preorder)
  (labels ((aux (cont)
             (map-children #'aux cont)
             (%cleanup cont)))
    (aux (start datum))))

(defun replace-datum (new old) (%map-uses (%use-replacer new) old))

(defun replace-terminator (new old)
  (%delete-terminator old)
  (let ((cont (continuation old)))
    (setf (%terminator cont) new))
  (replace-datum new old))

(defgeneric insert-before (new instruction))
(defmethod insert-before ((new bind) (inst instruction))
  (assert (not (continuation new)))
  (let ((prev (prev inst)))
    (setf (%prev new) prev (%next new) inst (%prev inst) new
          (%continuation new) (%continuation inst))
    (when prev (setf (%next prev) new))))

(defgeneric insert-after (new instruction))
(defmethod insert-after ((new bind) (inst bind))
  (assert (not (continuation new)))
  (let ((next (next inst)))
    (setf (%prev new) inst (%next new) next (%next inst) new
          (%continuation new) (%continuation inst))
    (when next (setf (%prev next) new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Building
;;;

(defclass builder ()
  ((%insert-point :initarg :insert-point :accessor %insert-point)))

(defgeneric build-before (builder instruction))
(defmethod build-before ((builder builder) (inst instruction))
  (let ((prev (prev inst)))
    (setf (%insert-point builder) (if (null prev) (continuation prev) prev))))

(defgeneric build-after (builder bind))
(defmethod build-after ((builder builder) (inst bind))
  (setf (%insert-point builder) inst))

(defgeneric build-continuation (builder continuation))
(defmethod build-continuation ((builder builder) (cont continuation))
  (setf (%insert-point builder) cont))

(defgeneric %build (insert-point inst))
(defmethod %build ((ip continuation) (inst bind))
  (setf (%prev inst) nil (%next inst) (%start ip)
        (%start ip) inst (%continuation inst) ip))
(defmethod %build ((ip continuation) (inst terminator))
  (assert (not (builtp ip)))
  (setf (%prev inst) nil (%continuation inst) ip
        (%start ip) inst (%terminator ip) inst))
(defmethod %build ((ip bind) (inst bind)) (insert-after inst ip))
(defmethod %build ((ip bind) (inst terminator))
  (assert (not (next ip)))
  (setf (%prev inst) ip (%next ip) inst
        (%continuation inst) (%continuation ip)
        (%terminator (continuation ip)) inst))

(defun build (builder instruction)
  (%build (%insert-point builder) instruction)
  (setf (%insert-point builder) instruction))

(defvar *builder*)

(defmacro with-builder ((&rest options) &body body)
  (declare (ignore options)) ; maybe later
  `(let ((*builder* (make-instance 'builder)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inlining and outlining
;;;

;; Destroy FUNCTION by making it into a continuation that is a child of PARENT,
;; a continuation in some other function. The new continuation takes
;; (combinand . enclosed) as its argument.
;; The new continuation is returned but not actually inserted into control flow
;; and the caller is expected to be doing that.
;; NOTE: Would be a little easier if function was tracked through parentage?
#+(or)
(defun %inline (function parent)
  (let ((new-function (function parent))
        (start (start function))
        (enclosed (enclosed function))
        (root (root function)))
    (map-continuations (lambda (cont) (setf (%function cont) new-function))
                       start)
    (replace-datum parent root)
    (with-continuation-assembly
        ((new-start param parent (name function) (gensym "COMBINAND.ENCLOSED"))
          (:= car (car param) (name (parameter start)))
          (:= cdr (cdr param) (name enclosed))
          (continue start car))
      (setf (%parent start) new-start)
      (replace-datum cdr enclosed)
      new-start)))

;;; TODO: When we can, i.e. we are contifying, it would be good to avoid the
;;; fairly major consing undertaking that is copying a function.
#+(or)
(defun inline (function return-point)
  (%inline (copy function) return-point))
