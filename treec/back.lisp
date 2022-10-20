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
  (let* ((ptree (ptree operative)) (eparam (eparam operative))
         (cf (make-instance 'asm:cfunction
               :ptree ptree :eparam eparam :cmodule cmodule))
         (static-env-var (static-env-var operative))
         ;; Set (list) of variables we're binding that need to be in a cell
         ;; because they are $set! in a closure.
         ;; (They also need to be in a cell if the environment is reified,
         ;;  so in that case we don't bother computing this.)
         (enclosed-set (if static-env-var
                           nil
                           (intersection (ptree-symbols (cons eparam ptree))
                                         (enclosed-sets (body operative)))))
         (local-env-var (if static-env-var (env-var operative) nil))
         (locals (op-bindings ptree eparam local-env-var enclosed-set))
         ;; next register available
         (next-index (length locals))
         (ptree-nstack
           (progn
             (asm:emit-label cf (asm:gep cf))
             (gen-operative-bindings cf ptree eparam locals local-env-var)))
         (cep-nstack
           (progn
             ;; Jump over the CEP to the body.
             ;; It might be faster to choose the more common of the CEP or GEP to
             ;; proceed into the LEP... but that's probably premature optimization.
             (asm:assemble cf o:jump (asm:lep cf))
             (asm:emit-label cf (asm:cep cf))
             (gen-cep cf ptree eparam locals local-env-var))))
    (asm:emit-label cf (asm:lep cf))
    (when static-env-var
      ;; Close over the static environment if we must.
      ;; Note that ptree.lisp needs this to be at closure 0, hence forcing it
      ;; to be the first closed over variable.
      (asm:closure-index cf static-env-var))
    (let ((*regs-hwm* next-index)
          (*stack-hwm* (max ptree-nstack cep-nstack)))
      (translate (body operative)
                 (make-instance 'context
                   :cfunction cf :link-env link-env :locals locals
                   :nregs next-index :nstack 0))
      (setf (asm:nlocals cf) *regs-hwm* (asm:nstack cf) *stack-hwm*))
    cf))

;;; This is separated out from the TRANSLATE on REF because it's also used by the
;;; translation for operative nodes.
(defun symbol-binding (symbol context)
  (let ((lpair (assoc symbol (locals context)))
        (cf (cfunction context)))
    (cond ((not lpair) (asm:assemble cf 'o:closure (asm:closure-index cf symbol)))
          ((third lpair) ; cell
           (asm:assemble cf 'o:ref (second lpair)
             'o:slot-read (asm:constant-index cf class:cell) 0))
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

(defun translate-constant (value context)
  (when (valuep context)
    (mark-stack (1+ (nstack context)))
    (let ((cfunction (cfunction context)))
      (asm:assemble cfunction 'o:const (asm:constant-index cfunction value))
      (when (tailp context) (asm:assemble cfunction 'o:return)))))

(defmethod translate ((node const) context)
  (translate-constant (value node) context))

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
  ;; Call (unwrap combine). It's important to use standard combiners for this stuff,
  ;; so that serialized modules can look them up.
  (let ((cf (cfunction context))
        ($combine (i:unwrap (i:lookup 'syms::combine baselib:*base*))))
    (asm:assemble cf 'o:const (asm:constant-index cf $combine)
      'o:const (asm:constant-index cf *empty-env*))
    (translate (combiner node) (context context :valuep t :tailp nil :new-stack 2))
    (translate (combinand node) (context context :valuep t :tailp nil :new-stack 3))
    (translate (env node) (context context :valuep t :tailp nil :new-stack 4))
    (asm:assemble cf (if (tailp context) 'o:tail-call 'o:call) 4)
    (unless (valuep context) (asm:assemble cf 'o:drop))))

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
      (asm:assemble cf 'o:slot-read (asm:constant-index cf class:applicative) 0)
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node wrap) context)
  (when (valuep context)
    (let ((cf (cfunction context)))
      (translate (unwrap node) (context context :valuep t :tailp nil))
      (asm:assemble cf 'o:construct (asm:constant-index cf class:applicative))
      (when (tailp context) (asm:assemble cf 'o:return)))))

(defmethod translate ((node ifn) context)
  (let* ((cf (cfunction context))
         (thenl (asm:make-label))
         (tailp (tailp context))
         (mergel (unless tailp (asm:make-label)))
         (rcontext (context context :new-stack 1)))
    (translate (if-cond node) (context context :valuep t :tailp nil))
    (asm:assemble cf 'o:dup 'o:check-class (asm:constant-index cf class:boolean))
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
      (cond ((zerop (length free))
             ;; The operative doesn't actually close over anything, so it can
             ;; simply be a constant.
             ;; asm:link will (eventually) replace the cfunction with the actual
             ;; code in the constant vector.
             (mark-stack (+ 1 (nstack context)))
             (asm:assemble cf 'o:const (asm:constant-index cf opcf)))
            (t
             (mark-stack (+ (length free) (nstack context)))
             (loop for var in free
                   do (symbol-binding var context))
             (asm:assemble cf 'o:enclose (asm:constant-index cf opcf))))
      (when (tailp context) (asm:assemble cf 'o:return)))))

;;; Given a ptree and a node providing its value, return an alist of
;;; (ptree . node) that will need to be bound-or-set. This takes apart list
;;; nodes only used for binding, to avoid actually consing lists.
(defun pick-apart-bind (ptree node)
  (etypecase ptree
    (null
     (if (or (and (typep node 'const) (null (value node)))
             (and (typep node 'listn) (null (elements node))))
         nil
         (acons ptree node nil)))
    ((or i:ignore symbol) (acons ptree node nil))
    (cons
     (if (typep node 'listn)
         (let ((elems (elements node)))
           (append (pick-apart-bind (car ptree) (first elems))
                   (pick-apart-bind (cdr ptree) (make-listn (rest elems)))))
         (acons ptree node nil)))))

(defmethod translate ((node letn) context)
  (let* ((ptree (ptree node))
         (picked (pick-apart-bind ptree (value node)))
         (cf (cfunction context))
         (body (body node))
         (enclosed-set (enclosed-sets body))
         (index (nregs context))
         (inner-envv (inner-env-var node))
         (eparam (dynenv-bind node))
         (outer-envv (static-env-var node))
         (outer-env-index (second (assoc outer-envv (locals context))))
         (new-locals (op-bindings ptree eparam inner-envv enclosed-set index))
         (ptree-stack 0)
         (estack 0))
    (assert (or (not (or inner-envv eparam)) outer-env-index))
    ;; Compute all the values.
    (loop for (_ . value) in picked
          for nstack from 0
          for ncontext = (context context :valuep t :tailp nil
                                          :new-stack nstack)
          do (translate value ncontext))
    ;; Bind 'em. We pushed left to right so now we go right to left.
    ;; As mentioned in the setn method below, we could rearrange the
    ;; evaluations arbitrarily if we were so inclined.
    (setf ptree-stack
          (loop for (ptree) in (nreverse picked)
                for nstack downfrom (length picked)
                maximizing (+ nstack (gen-ptree cf ptree new-locals))))
    ;; Bind environment crap.
    (when eparam
      ;; Bind the dynenv parameter.
      ;; NOTE: Since treec is simple, we know the local environment, i.e.
      ;; what's in outer-env-index, is never cloistered (in a cell).
      ;; TODO: If the eparam is never actually mutated, we could just use it
      ;; as an alias - like push a binding to an index that's exactly the
      ;; same as the outer index.
      (let ((eparam-info (assoc eparam new-locals)))
        (asm:assemble cf 'o:ref outer-env-index)
        (when (third eparam-info) ; cell
          (asm:assemble cf 'o:construct (asm:constant-index cf class:cell)))
        (asm:assemble cf 'o:set index))
      (incf estack))
    (when inner-envv ; haveta reify
      ;; Get the parent environment
      (asm:assemble cf 'o:ref outer-env-index)
      ;; Grab all the locals to put into the reified environment.
      (let ((lin (if (typep eparam 'i:ignore)
                     (ptree-symbols ptree)
                     (list* eparam (ptree-symbols ptree)))))
        (loop for var in lin
              for index = (second (assoc var new-locals))
              do (asm:assemble cf 'o:ref index))
        (asm:assemble cf 'o:make-environment
          (asm:constant-index cf lin)
          'o:set index)
        (incf estack (1+ (length lin)))))
    (mark-regs (+ (nregs context) (length new-locals)))
    (mark-stack (+ (nstack context) ptree-stack estack))
    ;; At this point the stack is where it was when the $let was entered,
    ;; so the body context doesn't need new stack.
    (translate body
               (context context :new-locals new-locals
                                :new-regs (length new-locals)))))

(defmethod translate ((node setn) context)
  (let ((pairs (pick-apart-bind (ptree node) (value node))))
    ;; Ensure the values are all evaluated before setting anything.
    ;; (We're free to reorder these evaluations, but treec isn't really
    ;;  smart enough to make any use of that.)
    (loop for (_ . value) in pairs
          for nstack from 0
          for ncontext = (context context :valuep t :tailp nil :new-stack nstack)
          do (translate value ncontext))
    ;; Now for ($set! (a b c) (list x y z)) we have x y z on the stack.
    ;; So set c, then b, etc.
    (mark-stack
     (+ (nstack context)
        (loop with cf = (cfunction context)
              with locals = (locals context)
              for (ptree) in (nreverse pairs)
              for nstack downfrom (length pairs)
              maximizing (+ nstack (set-ptree cf ptree locals))))))
  (translate-constant i:inert context))
