(defpackage #:burke/quickc
  (:use #:cl)
  (:shadow #:compile #:tailp)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:syms #:burke/interpreter/syms)
                    (#:o #:burke/vm/ops)
                    (#:vm #:burke/vm)
                    (#:asm #:burke/vm/asm)
                    (#:info #:burke/info)
                    (#:cenv #:burke/cenv))
  (:export #:compile)
  (:export #:compilation-module))

(in-package #:burke/quickc)

;;; A locally bound variable.
(defclass local-binding (cenv:binding)
  (;; What function is it local to?
   (%cfunction :initarg :cfunction :reader cfunction)
   ;; Register assignment.
   (%index :initarg :index :reader index)))

;;; Represents information about something that has just been compiled.
;;; Name kinda sucks.
(defclass result ()
  ((%info :initarg :info :reader info :type info:info)
   ;; How many locals it binds.
   (%nlocals :initarg :nlocals :reader nlocals :type (and unsigned-byte fixnum))
   ;; How much stack space it needs.
   (%nstack :initarg :nstack :reader nstack :type (and unsigned-byte fixnum))))

(defun result (info nlocals nstack)
  (make-instance 'result :info info :nlocals nlocals :nstack nstack))

(defclass context ()
  ((%cfunction :initarg :cfunction :reader cfunction :type asm:cfunction)
   ;; Are we expected to produce a value? This is important for making sure that
   ;; the stack is in the same state after either branch of an $if, for example.
   (%valuep :initform t :initarg :valuep :reader valuep :type boolean)
   ;; Tail context? Note that (not valuep) implies (not tailp).
   (%tailp :initform t :initarg :tailp :reader tailp :type boolean)
   ;; Local index bound to the current environment.
   (%env-index :initarg :env-index :reader env-index :type (and unsigned-byte fixnum))
   ;; Next index to use if we need to bind.
   (%nlocals :initarg :nlocals :reader nlocals :type (and unsigned-byte fixnum))))

(defun context (context &key (valuep (valuep context))
                          (tailp (if valuep (tailp context) nil))
                          (new-locals 0))
  (make-instance 'context
    :cfunction (cfunction context) :tailp tailp :valuep valuep
    :nlocals (+ (nlocals context) new-locals) :env-index (env-index context)))

(defun assemble (context &rest items)
  (apply #'asm:assemble (cfunction context) items))

(defun constant-index (value context)
  (asm:constant-index (asm:cmodule (cfunction context)) value))

(defun compile (ptree eparam body cenvironment environment)
  (let* ((result (compile-operative ptree eparam body cenvironment
                                    (make-instance 'asm:cmodule)))
         (cfunction (info:data (info result)))
         (code (asm:link cfunction)))
    (vm:enclose code
                (coerce
                 (loop for item across (asm:closed cfunction)
                       if (eq item cenvironment)
                         collect environment
                       else if (symbolp item)
                              collect (i:lookup item environment)
                       else do (error "How did ~a get in the closure vector?"
                                      item))
                 'simple-vector))))

(defun compile-combiner (combiner cenv)
  (etypecase combiner
    (i:derived-operative
     (compile (i:ptree combiner) (i:eparam combiner)
              (i:body combiner) cenv (i:env combiner)))
    (i:applicative
     (i:wrap (compile-combiner (i:unwrap combiner) cenv)))
    (i:combiner combiner)))

(defun compilation-module ()
  "Return a Burke environment with bindings for the quick compiler."
  (i:make-fixed-environment
   '(syms::compile)
   (list (i:wrap (i:make-builtin-operative
                  (lambda (env combinand)
                    (declare (ignore env))
                    (destructuring-bind (combiner cenv) combinand
                      (compile-combiner combiner cenv)))
                  'syms::compile)))))

(defun linearize-ptree (ptree)
  (etypecase ptree
    ((or null i:ignore) nil)
    (symbol (list ptree))
    (cons (append (linearize-ptree (car ptree)) (linearize-ptree (cdr ptree))))))

(defun compile-operative (ptree eparam body cenv module)
  (let* ((cf (make-instance 'asm:cfunction
               :cmodule module :ptree ptree :eparam eparam))
         ;; All quick-compiled functions close over their static environment,
         ;; because quickc is not smart enough to remove it.
         (_ (asm:closure-index cf cenv))
         (cenv (if (symbolp eparam)
                   (cenv:augment1 cenv
                                  (list (cons eparam
                                              (make-instance 'local-binding
                                                :cfunction cf
                                                :index 1
                                                :info (info:default-info)))))
                   cenv)))
    (declare (ignore _))
    (multiple-value-bind (bindings context nlocals nstack) (gen-ptree cf ptree)
      #+(or)(setf (asm:sep cf) (asm:nbytes (cfunction context)))
      ;; Set up the current environment to be in index 2.
      ;; We assume the closed over environment is in closure 0.
      (assemble context 'o:closure 0)
      (let* ((estack
               (cond ((and (typep ptree '(or null i:ignore))
                           (typep eparam 'i:ignore))
                      (assemble context 'o:set 2)
                      1)
                     ((and (typep ptree '(and symbol (not null))) (symbolp eparam))
                      (assemble context 'o:ref 0 'o:ref 1
                        'o:make-environment (constant-index (list ptree eparam) context))
                      3)
                     ((and (typep ptree '(and symbol (not null))) (typep eparam 'i:ignore))
                      (assemble context 'o:ref 0
                        'o:make-environment (constant-index (list ptree) context))
                      2)
                     ((symbolp eparam)
                      (assemble context 'o:ref 1)
                      (let* ((lin (linearize-ptree ptree))
                             (llin (length lin)))
                        (loop repeat llin
                              for i from 3
                              do (assemble context 'o:ref i))
                        (assemble context 'o:make-environment
                          (constant-index (list* eparam (linearize-ptree lin)) context)
                          'o:set 2)
                        (+ 2 llin)))
                     (t
                      (let* ((lin (linearize-ptree ptree))
                             (llin (length lin)))
                        (loop repeat llin
                              for i from 3
                              do (assemble context 'o:ref i))
                        (assemble context 'o:make-environment (constant-index lin context)
                          'o:set 2)
                        (+ 1 llin)))))
             ;; Compile the body.
             (body (compile-seq body (cenv:augment1 cenv bindings) context))
             (info (make-instance 'info:local-operative :data cf))
             (nlocals (+ 3 nlocals (nlocals body)))
             (nstack (max nstack estack (nstack body))))
        (setf (asm:nlocals cf) nlocals (asm:nstack cf) nstack)
        (result info 0 0)))))

;;; Generate code to do argument parsing.
;;; Return four values:
;;; A list of bindings. A context. And the amounts of registers and stack used.
(defun gen-ptree (cfunction ptree)
  (let* ((context
          (make-instance 'context
            :cfunction cfunction :env-index 2 :nlocals 3)))
    (etypecase ptree
      (null
       (assemble context 'o:ref 0 'o:err-if-not-null)
       (values nil context 0 1))
      (symbol
       ;; Just use index 0 and don't bind anything.
       (values (list (cons ptree (make-instance 'local-binding
                                   :cfunction cfunction
                                   :index 0
                                   :info (info:default-info))))
               context 0 0))
      (i:ignore (values nil context 0 0))
      (cons ; this is the hard part.
       (let* ((vars (linearize-ptree ptree))
              (nvars (length vars))
              (context (context context :new-locals nvars))
              (next-var-local 3))
         (labels ((next-var-local () (prog1 next-var-local (incf next-var-local)))
                  (aux (ptree next-temp-local)
                    (etypecase ptree
                      (i:ignore (values nil 0))
                      (null (assemble context 'o:err-if-not-null) (values nil 0))
                      (symbol
                       (let ((l (next-var-local)))
                         (assemble context 'o:set l)
                         (values (list (cons ptree (make-instance 'local-binding
                                                     :cfunction cfunction
                                                     :index l
                                                     :info (info:default-info))))
                                 0)))
                      (cons
                       (let ((cons-local next-temp-local))
                         (assemble context 'o:set cons-local
                           'o:ref cons-local 'o:err-if-not-cons
                           'o:ref cons-local 'o:car)
                         (multiple-value-bind (car-locals car-temps)
                             (aux (car ptree) (1+ next-temp-local))
                           (assemble context 'o:ref cons-local 'o:cdr)
                           (multiple-value-bind (cdr-locals cdr-temps)
                               ;; We can just stomp on any temporaries the
                               ;; car ptree made, and on the cons now that we
                               ;; don't need to do anything else with it.
                               (aux (cdr ptree) next-temp-local)
                             (values (append car-locals cdr-locals)
                                     (max (1+ car-temps) cdr-temps)))))))))
           (assemble context 'o:ref 0)
           (multiple-value-bind (bindings ntemps) (aux ptree (+ 3 nvars))
             (values bindings context (+ 3 ntemps) 1))))))))

(defun compile-seq (body cenv context)
  (cond ((null body) (compile-constant i:inert context))
        ((null (cdr body)) (compile-form (car body) cenv context))
        (t
         (let ((nlocals 0) (nstack 0))
           (loop with context = (context context :valuep nil)
                 for form in (butlast body)
                 for result = (compile-form form cenv context)
                 do (setf nlocals (max nlocals (nlocals result))
                          nstack (max nstack (nstack result))))
           (let ((final (compile-form (first (last body)) cenv context)))
             (setf nlocals (max nlocals (nlocals final))
                   nstack (max nstack (nstack final)))
             (result (info final) nlocals nstack))))))

(defun compile-constant (value context)
  (cond ((valuep context)
         (assemble context 'o:const (constant-index value context))
         (when (tailp context) (assemble context 'o:return))
         (result (make-instance 'info:constant :value value) 0 1))
        (t (result (make-instance 'info:constant :value value) 0 0))))

(defun compile-form (form cenv context)
  (typecase form
    (symbol (compile-symbol form cenv context))
    (cons (compile-cons (car form) (cdr form) cenv context))
    (t (compile-constant form context))))

(defun compile-symbol (symbol cenv context)
  (let* ((binding (cenv:lookup symbol cenv))
         (info (if binding
                   (cenv:info binding)
                   (info:default-info))))
    (when (null binding)
      (warn "Unknown variable ~a" symbol))
    (cond ((not (valuep context)) (return-from compile-symbol (result info 0 0)))
          ((and (typep binding 'local-binding)
                (eq (cfunction binding) (cfunction context)))
           ;; Actually local
           (assemble context 'o:ref (index binding)))
          (t ; have to close
           (assemble context 'o:closure
             ;; FIXME: Only recording the symbol is probably wrong in general,
             ;; since we might want to close over things from outside the
             ;; direct static environment at some point.
             (asm:closure-index (cfunction context) symbol))))
    (when (tailp context) (assemble context 'o:return))
    (result info 0 1)))

(defun compile-cons (combinerf combinand cenv context)
  (let* ((combinerr (compile-form combinerf cenv (context context :valuep t :tailp nil)))
         (combineri (info combinerr))
         (cres
           (cond ((and (typep combineri 'info:known-operative)
                       ;; We know what this is, so try something special.
                       ;; This can return NIL, in which case we have to try
                       ;; something else.
                       (compile-known-operation (info:name combineri)
                                                combinand cenv context)))
                 (t (compile-general-combination combinand cenv context)))))
    (result (info cres)
            (max (nlocals cres) (nlocals combinerr))
            (+ (nstack cres) (nstack combinerr)))))

;;; The combiner is on the stack, and we don't know shit about it.
;;; This is basically a worst case scenario and might warrant a style warning,
;;; or at least some kind of "hey! your performance is going to crap" note.
(defun compile-general-combination (combinand cenv context)
  (declare (ignore cenv))
  (let ((res
          (compile-constant combinand (context context :valuep t :tailp nil))))
    (assemble context 'o:ref (env-index context))
    (cond ((tailp context) (assemble context 'o:tail-combine))
          ((valuep context) (assemble context 'o:combine))
          (t (assemble context 'o:combine 'o:drop)))
    ;; +1 for env reference.
    (result (info:default-info) (nlocals res) (+ (nstack res) 1))))
