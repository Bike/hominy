(in-package #:burke/treec)

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
   ;; A list (var index cellp)*
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
                &key (new-regs 0) (new-stack 0) new-locals
                  (valuep (valuep context)) (tailp (tailp context)))
  (make-instance 'context
    :cfunction (cfunction context) :link-env (link-env context)
    :locals (append new-locals (locals context))
    :nregs (+ (nregs context) new-regs) :nstack (+ (nstack context) new-stack)
    :valuep valuep :tailp (and valuep tailp)))

;;; Generate code for a node. Called for effect.
(defgeneric translate (node context))

;;; Make and return a cfunction for the given operative node.
(defun translate-operative (operative link-env cmodule)
  (let* ((cf (make-instance 'asm:cfunction
               :ptree (ptree operative) :eparam (eparam operative)
               :cmodule cmodule))
         (static-env-var (static-env-var operative)))
    (asm:emit-label cf (asm:gep cf))
    (multiple-value-bind (locals ptree-nregs ptree-nstack)
        (gen-operative-bindings cf (ptree operative) (eparam operative)
                                (if static-env-var (env-var operative) nil))
      ;; Jump over the CEP to the body.
      ;; It might be faster to choose the more common of the CEP or GEP to
      ;; proceed into the LEP... but that's probably premature optimization.
      (asm:assemble cf o:jump (asm:lep cf))
      (asm:emit-label cf (asm:cep cf))
      (let ((cep-nstack (gen-cep cf (ptree operative) (eparam operative) locals
                                 (if static-env-var (env-var operative) nil))))
        (asm:emit-label cf (asm:lep cf))
        (when static-env-var
          ;; if the environment is reified, close over the static environment.
          ;; Note that ptree.lisp needs this to be at closure 0, hence forcing it
          ;; to be the first closed over variable.
          (asm:closure-index cf static-env-var))
        (let ((*regs-hwm* ptree-nregs)
              (*stack-hwm* (max ptree-nstack cep-nstack)))
          (translate (body operative)
                     (make-instance 'context
                       :cfunction cf :link-env link-env :locals locals
                       :nregs ptree-nregs :nstack 0))
          (setf (asm:nlocals cf) *regs-hwm* (asm:nstack cf) *stack-hwm*)))
      cf)))

(defun linearize-ptree (ptree)
  (etypecase ptree
    ((or null i:ignore) nil)
    (symbol (list ptree))
    (cons (append (linearize-ptree (car ptree)) (linearize-ptree (cdr ptree))))))

;;; This is separated out from the TRANSLATE on REF because it's also used by the
;;; translation for operative nodes.
(defun symbol-binding (symbol context)
  (let ((lpair (assoc symbol (locals context)))
        (cf (cfunction context)))
    (cond ((not lpair) (asm:assemble cf 'o:closure (asm:closure-index cf symbol)))
          ((third lpair) ; cell
           (asm:assemble cf 'o:ref (second lpair) 'o:cell-ref))
          (t (asm:assemble cf 'o:ref (second lpair))))))

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
  (or (translate-primitive node context)
      (translate-call node context)
      (translate-general-combination node context)))

;;; If the combiner is a primitive operative, and the combinand is a list of valid length,
;;; compile down to VM operations instead of an actual combination.
;;; If this is not possible, return NIL.
;;; We do this in the backend, rather than having an IR node for primitives, because the kind of
;;; optimizations the frontend does are insensitive to whether something is a VM primitive.
;;; It would be an abstraction leak.
(defun translate-primitive (node context)
  (let ((combinern (combiner node)) (combinandn (combinand node)))
    (if (typep combinandn 'listn)
        (let* ((args (elements combinandn)) (nargs (length args))
               (cf (cfunction context)))
          (multiple-value-bind (arity op) (primitive combinern)
            (cond ((and arity (= arity nargs))
                   ;; we have a primitive and the argument count is correct, so we're doing this.
                   ;; First, if we're not in a value context do nothing, as all primitives are
                   ;; side-effect-free, at least for the moment.
                   (when (valuep context)
                     ;; We translate the combiner and env nodes in case they have side effects.
                     ;; Translating the environment before the args is ok, since the order of
                     ;; evaluation of an applicative's arguments is deliberately unspecified.
                     (translate combinern (context context :valuep nil))
                     (translate (env node) (context context :valuep nil))
                     ;; Next we translate the arguments, one at a time.
                     (loop for ns from 0
                           for arg in args
                           do (translate arg (context context :valuep t :tailp nil :new-stack ns)))
                     ;; Do the actual operation.
                     (asm:assemble cf op)
                     ;; And finally, if we're in a tail context, return.
                     (when (tailp context) (asm:assemble cf 'o:return)))
                   t)
                  (t nil))))
        nil)))

(defun translate-call (node context)
  (let ((combinand (combinand node))
        (cf (cfunction context)))
    (cond
      ((typep combinand 'listn) ; generate a call
       (translate (combiner node) (context context :valuep t :tailp nil))
       (translate (env node) (context context :valuep t :tailp nil :new-stack 1))
       ;; Translate the arguments
       (loop for i from 2 for node in (elements combinand)
             for ctxt = (context context :new-stack i :valuep t :tailp nil)
             do (translate node ctxt))
       (cond ((tailp context)
              (asm:assemble cf 'o:tail-call (1+ (length (elements combinand)))))
             (t
              (asm:assemble cf 'o:call (1+ (length (elements combinand))))
              (unless (valuep context)
                (asm:assemble cf 'o:drop))))
       t)
      (t nil))))

(defun translate-general-combination (node context)
  (translate (combiner node) (context context :valuep t :tailp nil))
  (translate (combinand node) (context context :valuep t :tailp nil :new-stack 1))
  (translate (env node) (context context :valuep t :tailp nil :new-stack 2))
  (asm:assemble (cfunction context) (if (tailp context) 'o:tail-combine 'o:combine))
  (unless (valuep context) (asm:assemble (cfunction context) 'o:drop)))

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
         (thenl (asm:make-label))
         (tailp (tailp context))
         (mergel (unless tailp (asm:make-label)))
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
           (free (free node)))
      (mark-stack (+ (length free) (nstack context)))
      (loop for var in free
            do (symbol-binding var context))
      (asm:assemble cf 'o:enclose (asm:constant-index cf opcf))
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node letn) orig-context)
  (loop with context = (context orig-context :valuep t :tailp nil)
        with cf = (cfunction context)
        for ptree in (ptrees node)
        for valuen in (value-nodes node)
        do (translate valuen context)
        append (multiple-value-bind (bindings next-local nstack)
                   (gen-ptree (cfunction context) ptree (nregs context) (inner-env-var node))
                 (setf context (context context :new-regs (- next-local (nregs context)) ; goofy
                                                :new-stack nstack :valuep t :tailp nil))
                 bindings)
          into bindings
        finally (let* ((outer-envv (static-env-var node))
                       (oeb (dynenv-bind node))
                       (inner-envv (inner-env-var node))
                       (cellp inner-envv)
                       (outer-env-index
                         (when (or oeb inner-envv)
                           (or (second (assoc outer-envv (locals context)))
                               (error "?? Reified environment ~a missing" outer-envv)))))
                  (when oeb
                    ;; NOTE: Since treec is simple, we know the local environment, i.e.
                    ;; what's in outer-env-index, is never cloistered (in a cell).
                    ;; TODO: If the OEB is never actually mutated, we could just use it
                    ;; as an alias - like push a binding to an index that's exactly the
                    ;; same as the outer index.
                    (asm:assemble cf 'o:ref outer-env-index)
                    (when cellp (asm:assemble cf 'o:make-cell))
                    (asm:assemble cf 'o:set (nregs context))
                    (push (list oeb (nregs context) cellp) bindings)
                    (setf context (context context :new-regs 1 :new-stack 1)))
                  (when inner-envv ; haveta reify
                    (asm:assemble cf 'o:ref outer-env-index)
                    ;; get the dynenv for reification
                    (when oeb (asm:assemble cf 'o:ref outer-env-index))
                    (loop for (_1 index _2) in bindings
                          do (asm:assemble cf 'o:ref index))
                    (asm:assemble cf 'o:make-environment
                      (asm:constant-index cf (if oeb
                                                 (cons oeb (mapcar #'car bindings))
                                                 (mapcar #'car bindings)))
                      'o:set (nregs context))
                    ;; just to reiterate - local environments never need to be in cells.
                    (push (list inner-envv (nregs context) nil) bindings)
                    ;; This actually overestimates the stack by 1 if the oeb also exists. FIXME
                    (setf context (context context :new-regs 1 :new-stack (1+ (length bindings))))))
                (mark-regs (nregs context))
                (mark-stack (nstack context))
                (translate (body node)
                           (context context :new-locals bindings
                                            :valuep (valuep orig-context)
                                            :tailp (tailp orig-context)))))
