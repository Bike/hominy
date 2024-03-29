(in-package #:hominy/ir)

;;; Assembly is a syntax for building continuations and whole functions in a
;;; hopefully reasonably simple way.
;;; Quick syntax:
;;; (assemble (namespec namespec namespec) cont)
;;; (assemble-continuation . cont)
;;;
;;; where:
;;; cont := (namespec (namespec) (cont*) . asmbody)
;;; namespec := SYMBOL | (SYMBOL FORM)
;;; asmbody := (binding* terminator)
;;; binding := (ASSIGN namespec instruction)
;;; terminator := instruction
;;; input := SYMBOL | instruction
;;; instruction := (SYMBOL INPUT*)
;;; where ASSIGN is the symbol := used in a non-BNF literal way.
;;;
;;; A namespec specifies a name in two ways: as a Lisp binding that can be
;;; referred to, and the name actually associated with the object. The former
;;; must be constant but not the latter. If a namespec is a symbol, it means
;;; to use that for the binding and (as a constant) the latter. If a namespec
;;; is a name and a form it means use those for the binding and name.
;;;
;;; ASSEMBLE's three names are the name of the function, the name of the
;;; enclosed variable, and the name of the return continuation. The two names
;;; in a cont are the name of the continuation and the name of its parameter.
;;; The name in a bind is the name of the binding.
;;; The extra continuations in a cont are the names of children. A continuation
;;; may refer to its children, its parent's children, itself, and the return
;;; continuation for the function. And maybe ancestors? I dunno.
;;; The instruction has the name of the instruction class, and then the rest
;;; are the inputs to the instruction. A symbol means a reference to another
;;; binding, and a cons is interpreted as an anonymous node. Bound instructions
;;; may refer to other instructions or themselves without ordering restrictions.

(defun %namespec (namespec)
  (if (consp namespec)
      (values (first namespec) (second namespec))
      (values namespec `',namespec)))

(defmacro build-instruction (name class &rest inputs)
  (let ((inputs (loop for input in inputs
                      collect (if (symbolp input)
                                  input
                                  `(build-instruction nil ,@input)))))
    `(make-instruction ',class ,name ,@inputs)))

;;; Given a list of (:= namespec instruction), return a list of
;;; (lispname iname ,@instruction).
(defun normalize-bindings (bindings)
  (flet ((normalize-binding (binding)
           (destructuring-bind (ass namespec instruction) binding
             (assert (eq ass :=))
             (multiple-value-bind (lispname iname) (%namespec namespec)
               (list* lispname iname instruction)))))
    (mapcar #'normalize-binding bindings)))

;;; Bind all the bindings. Because they can be recursive, we have to do some
;;; shenanigans. Specifically, we make everything first, then mutate the uses.
;;; In order to make the macroexpansion not huge, we go through some effort
;;; to just use build-instruction when possible.
(defmacro with-bindings-built ((&rest bindings) &body body)
  (let* ((bindings (normalize-bindings bindings))
         (lispnames (mapcar #'first bindings))
         sets)
    (flet ((simple-binding-p (subnames inputs)
             ;; Can we use build-instruction? To do so, no inputs can be from
             ;; subsequent instructions.
             ;; (We could technically do better by reordering if necessary, but
             ;;  that would be kind of over the top.)
             (loop for input in inputs
                   never (member input subnames)))
           (simple-binding (lispname iname iclass inputs)
             (list `(,lispname (build-instruction ,iname ,iclass ,@inputs))))
           (complex-binding (lispname iname iclass inputs)
             (let ((usenames
                     (loop for input in inputs
                           collect (if (symbolp input)
                                       (make-symbol (symbol-name input))
                                       (gensym "USE")))))
               (loop for input in inputs for usename in usenames
                     for form = (if (symbolp input)
                                    input
                                    `(build-instruction nil ,@input))
                     do (push `((%definition ,usename) ,form) sets)
                        (push `((%user ,usename) ,lispname) sets))
               (append
                (loop for usename in usenames
                      collect `(usename (make-instance 'use)))
                (list `(,lispname
                        (make-instance ',iclass
                          :name ,iname :uinputs (list ,@usenames))))))))
      `(let* (,@(loop for subnames on lispnames
                      for (lispname iname iclass . inputs) in bindings
                      nconc (if (simple-binding-p subnames inputs)
                                (simple-binding lispname iname iclass inputs)
                                (complex-binding
                                 lispname iname iclass inputs))))
         ,@(if sets `((setf ,@sets)) nil)
         ,@body))))

(defun %fix-parameter-uinputs (continuation)
  ;; any user of the continuation is a definition of its parameter.
  (let ((param (parameter continuation)))
    (map-users (lambda (user)
                 (let ((use (make-instance 'use
                              :definition user :user param)))
                   (%add-uinput use param)
                   (%add-use user use)))
               continuation)))

(defmacro %%assemble-continuation (cform (&rest children) &body body)
  (let ((childinfo
          (loop for child in children
                collect (destructuring-bind
                            (childname (paramname) (&rest gchildren) . asmbody)
                            child
                          (multiple-value-bind (rcname rcnameform)
                              (%namespec childname)
                            (multiple-value-bind (rpname rpnameform)
                                (%namespec paramname)
                              (list* rcname rcnameform rpname rpnameform
                                     gchildren asmbody))))))
        (bindings (butlast body))
        (terminator (first (last body))))
    `(let (;; Bind children (so they and this can refer to each other)
           ,@(loop for (rcname rcnameform _ rpnameform) in childinfo
                   for child = `(make-continuation ,rcnameform ,rpnameform)
                   collect `(,rcname ,child)))
       ;; Create children
       ,@(loop for (rcname _1 pname _3 gchildren . asmbody) in childinfo
               collect `(let ((,pname (parameter ,rcname)))
                          (%assemble-continuation ,rcname
                                                  ,gchildren ,@asmbody)))
       ;; Add children to this continuation
       ,@(loop for (rcname) in childinfo
               collect `(add-child ,cform ,rcname))
       ;; Build this continuation
       (with-bindings-built (,@bindings)
         (setf (%terminator ,cform) (build-instruction nil ,@terminator)))
       ;; Fix our parameter's inputs
       (%fix-parameter-uinputs ,cform))))

(defmacro %assemble-continuation (name (paramname) (&rest children)
                                 &body body)
  (multiple-value-bind (name nameform) (%namespec name)
    (multiple-value-bind (pname pnameform) (%namespec paramname)
      `(let* ((,name (make-continuation ,nameform ,pnameform))
              (,pname (parameter ,name)))
         (%%assemble-continuation ,name ,children ,@body)
         ,name))))

(defmacro assemble-continuation (name (paramname) parent (&rest children)
                                 &body body)
  (let ((psym (gensym "PARENT")))
    `(let ((,name
             (%assemble-continuation ,name (,paramname) (,@children) ,@body))
           (,psym ,parent))
       (setf (%parent ,name) ,psym)
       (add-child ,psym ,name)
       ,name)))

(defmacro assemble ((name enclosedname retname)
                    (contname (paramname) (&rest children) &body cont))
  (let ((gstart (gensym "START")))
    (multiple-value-bind (name nameform) (%namespec name)
      (multiple-value-bind (ename enameform) (%namespec enclosedname)
        (multiple-value-bind (rname rnameform) (%namespec retname)
          `(let* ((,ename (make-instance 'enclosed :name ,enameform))
                  (,rname
                    (make-continuation ,rnameform (gensym "RETURN-VALUE")))
                  (,name (make-instance 'function
                           :name ,nameform :enclosed ,ename :rcont ,rname))
                  (,gstart (%assemble-continuation ,contname (,paramname)
                               (,@children)
                             ,@cont)))
             (%fix-parameter-uinputs ,rname)
             (setf (%parent ,gstart) ,name (%start ,name) ,gstart
                   (%parent ,rname) ,gstart)
             (linearize-function ,name)
             ,name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifying existing IR
;;;

(defmacro replace-terminator (inst (instclass &rest inputs))
  (let ((ginst (gensym "INST"))
        (gmod (gensym "MODULE")))
    `(let* ((,ginst ,inst)
            (,gmod (module ,ginst)))
       (declare (ignorable ,gmod))
       (%replace-terminator ,ginst
                            (mod-build-instruction ,gmod
                                                   (,instclass ,@inputs))))))

(defun %replace-terminator (inst replacement)
  (let ((cont (continuation inst)))
    (setf (%terminator cont) replacement)
    ;; Remove all of the old terminator's USEs. This should trigger cleanup.
    (map-uses (lambda (use)
                (let ((user (user use)))
                  (%remove-uinput use user))
                (%remove-use inst use))
              inst)
    ;; Add a use for every continuation input of the replacement. Ugly/KLUDGE
    (dolist (input (inputs replacement))
      (when (typep input 'continuation)
        (let* ((param (parameter input))
               (use (make-instance 'use :definition replacement :user param)))
          (%add-use replacement use)
          (%add-uinput use param))))
    (map-uses (lambda (use) (setf (definition use) replacement)) inst)
    ;; Maintain the linearization.
    ;; FIXME: Can we do this to only the added instruction to save some time?
    (relinearize-function (function replacement)))
  replacement)

;;; this is like 
(defmacro mod-build-instruction (module (instclass &rest inputs))
  (let ((inputs (loop for input in inputs
                      collect (typecase input
                                (symbol input)
                                (atom `(constant ',input ,module))
                                ((cl:cons (eql quote))
                                 `(constant ,(second input) ,module))
                                (t
                                 `(mod-build-instruction
                                      ,module (,@input)))))))
    `(make-instruction ',instclass nil ,@inputs)))
