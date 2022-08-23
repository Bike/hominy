(in-package #:burke/treec)

;;; Magic marker used to indicate the static environment needs to be linked here.
(defvar *static-env-link-marker* (cons nil nil))

(defvar *regs-hwm*) ; how many locals have we used?
(defvar *stack-hwm*) ; and how much stack?

;;; Set the water mark higher if we need to.
;;; For MARK-STACK, we only really do this when we hit a "local maximum" - that is, we don't
;;; bother increasing it partway through compiling a combination, for example, just when we
;;; hit a ref or something whose compilation doesn't recurse.
(defun mark-regs (n) (setf *regs-hwm* (max *regs-hwm* n)))
(defun mark-stack (n) (setf *stack-hwm* (max *stack-hwm* n)))

(defclass context ()
  ((%cfunction :initarg :cfunction :reader cfunction :type asm:cfunction)
   (%link-env :initarg :link-env :reader link-env :type i:environment)
   (%locals :initarg :locals :reader locals :type list)
   (%nregs :initarg :nregs :reader nregs :type (integer 0))
   (%nstack :initarg :nstack :reader nstack :type (integer 0))
   (%valuep :initform t :initarg :valuep :reader valuep :type boolean)
   (%tailp :initform t :initarg :tailp :reader tailp :type boolean)))

(defmethod print-object ((ctxt context) stream)
  (print-unreadable-object (ctxt stream :type t)
    (when (valuep ctxt)
      (write (if (tailp ctxt) :tailp :valuep) :stream stream)
      (write-char #\Space stream))
    (format stream "~a ~d ~a ~d" :nregs (nregs ctxt) :nstack (nstack ctxt)))
  ctxt)

(defun context (context
                &key (new-regs 0) (new-stack 0)
                  (valuep (valuep context)) (tailp (tailp context)))
  (make-instance 'context
    :cfunction (cfunction context) :link-env (link-env context) :locals (locals context)
    :nregs (+ (nregs context) new-regs) :nstack (+ (nstack context) new-stack)
    :valuep valuep :tailp (and valuep tailp)))

;;; Generate code for a node. Called for effect.
(defgeneric translate (node context))

;;; Make and return a cfunction for the given operative node.
(defun translate-operative (operative link-env cmodule)
  (let* ((cf (make-instance 'asm:cfunction
               :plist (plist operative) :eparam (eparam operative)
               :cmodule cmodule))
         (closes-env-p (closes-env-p operative)))
    (multiple-value-bind (locals plist-nregs plist-nstack)
        (gen-operative-bindings cf (plist operative) (eparam operative)
                                (if closes-env-p (env-var operative) nil))
      (setf (asm:sep cf) (asm:nbytes cf))
      (when closes-env-p
        ;; if the environment is reified, close over the static environment.
        ;; Note that plist.lisp needs this to be at closure 0.
        (asm:closure-index cf *static-env-link-marker*))
      (let ((*regs-hwm* plist-nregs) (*stack-hwm* plist-nstack))
        (translate (body operative)
                   (make-instance 'context
                     :cfunction cf :link-env link-env :locals locals
                     :nregs plist-nregs :nstack 0))
        ;; We need at least two registers for the XEP to work.
        ;; FIXME: If we don't need a XEP we don't need those registers.
        (setf (asm:nlocals cf) (max 2 *regs-hwm*) (asm:nstack cf) *stack-hwm*)))
    cf))

(defun linearize-plist (plist)
  (etypecase plist
    ((or null i:ignore) nil)
    (symbol (list plist))
    (cons (append (linearize-plist (car plist)) (linearize-plist (cdr plist))))))

;;; This is separated out from the TRANSLATE on REF because it's also used by the
;;; translation for operative nodes, and when mutation is reintroduced, that will
;;; have to do something slightly different.
(defun symbol-binding (symbol context)
  (let ((lpair (assoc symbol (locals context)))
        (cf (cfunction context)))
    (cond (lpair (asm:assemble cf 'o:ref (cdr lpair)))
          (t (asm:assemble cf 'o:closure (asm:closure-index cf symbol))))))

(defmethod translate ((node ref) context)
  (when (valuep context)
    (mark-stack (1+ (nstack context)))
    (symbol-binding (ref-symbol node) context)
    (when (tailp context) (asm:assemble (cfunction context) 'o:return))))

(defmethod translate ((node link) context)
  (when (valuep context)
    (mark-stack (1+ (nstack context)))
    ;; Links are just constants, though they may be mutated.
    ;; When variable mutation is introduced, this will mean that the "constant" will be
    ;; the cell.
    ;; Possible TODO: Distinguish immutable constants. Really, "constant" might not be
    ;; the best term to use.
    (let ((cfunction (cfunction context)))
      (asm:assemble cfunction
        'o:const (asm:constant-index
                  cfunction (i:lookup (link-symbol node) (link-env context))))
      (when (tailp context) (asm:assemble cfunction 'o:return)))))

(defmethod translate ((node const) context)
  (when (valuep context)
    (mark-stack (1+ (nstack context)))
    (let ((cfunction (cfunction context)))
      (asm:assemble cfunction 'o:const (asm:constant-index cfunction (value node)))
      (when (tailp context) (asm:assemble cfunction 'o:return)))))

(defmethod translate ((node combination) context)
  (translate (combiner node) (context context :valuep t :tailp nil))
  (translate (combinand node) (context context :valuep t :tailp nil :new-stack 1))
  (translate (env node) (context context :valuep t :tailp nil :new-stack 2))
  (asm:assemble (cfunction context) (if (tailp context) 'o:tail-combine 'o:combine)))

(defmethod translate ((node seq) context)
  (loop with fecontext = (context context :valuep nil)
        for node in (for-effect node)
        do (translate node fecontext))
  (translate (final node) context))

(defmethod translate ((node listn) context)
  (when (valuep context)
    (let ((elemnodes (elements node))
          (cf (cfunction context)))
      (cond ((null elemnodes)
             ;; Constant
             (mark-stack (1+ (nstack context)))
             (asm:assemble cf 'o:const (asm:constant-index cf ())))
            (t
             (loop for i from 0 for node in elemnodes
                   for ctx = (context context :new-stack i :valuep t :tailp nil)
                   do (translate node ctx))
             (asm:assemble cf 'o:list (length elemnodes))))
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node unwrap) context)
  (when (valuep context)
    (let ((cf (cfunction context)))
      (translate (applicative node) (context context :valuep t :tailp nil))
      (asm:assemble cf 'o:unwrap)
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node ifn) context)
  (let* ((cf (cfunction context))
         (thenl (asm:make-label cf))
         (tailp (tailp context))
         (mergel (unless tailp (asm:make-label cf)))
         (rcontext (context context :new-stack 1)))
    (translate (if-cond node) (context context :valuep t :tailp nil))
    (asm:assemble cf 'o:dup 'o:err-if-not-bool)
    (mark-stack (+ 2 (nstack context)))
    (asm:assemble cf 'o:jump-if-true thenl)
    (translate (else node) rcontext)
    (unless tailp (asm:assemble cf 'o:jump mergel))
    (asm:emit-label cf thenl)
    (translate (then node) rcontext)
    (unless tailp (asm:emit-label cf mergel))))

(defmethod translate ((node operative) context)
  (when (valuep context)
    (let* ((cf (cfunction context))
           (opcf (translate-operative node (link-env context) (asm:cmodule cf)))
           (free (operative-free node))
           ;; If we're inside an ENCLOSE node, that pushed the static env and left
           ;; the rest to us. (KLUDGE)
           (nclosed (if (closes-env-p node) (1+ (length free)) (length free))))
      (mark-stack (+ nclosed (nstack context)))
      (loop for var in free
            do (symbol-binding var context))
      (asm:assemble cf 'o:enclose (asm:constant-index cf opcf))
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node enclose) context)
  (when (valuep context)
    ;; See KLUDGE in TRANSLATE OPERATIVE
    (symbol-binding (env-var node) context)
    (translate (operative node) context)))