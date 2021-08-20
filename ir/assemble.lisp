(in-package #:burke/ir)

;;; Assembly is a syntax for building continuations and whole functions in a
;;; hopefully reasonably simple way.
;;; Quick syntax:
;;; (assemble-continuation . cont)
;;; (assemble (namespec namespec namespec) cont)
;;; namespec := SYMBOL | (SYMBOL FORM)
;;; fbody := (cont*)
;;; cont := ((namespec namespec) (cont*) . asmbody)
;;; asmbody := (bind* term)
;;; bind := (ASSIGN namespec instruction) | instruction
;;; term := instruction
;;; instruction := (SYMBOL FORM*)
;;; where ASSIGN is the symbol := used in a non-BNF literal way.
;;; A namespec specifies a name in two ways: as a Lisp binding that can be
;;; referred to, and the name actually associated with the object. The former
;;; must be constant but not the latter. If a namespec is a symbol, it means
;;; to use that for the binding and (as a constant) the latter. If a namespec
;;; is a name and a form it means use those for the binding and name.
;;; ASSEMBLE's three names are the name of the function, the name of the
;;; enclosed variable, and the name of the return continuation. The two names
;;; in a cont are the name of the continuation and the name of its parameter.
;;; The name in a bind is the name of the binding.
;;; The extra continuations in a cont are the names of children. A continuation
;;; may refer to its children, its parent's children, itself, and the return
;;; continuation for the function. And maybe ancestors? I dunno.
;;; The instruction has the name of the instruction class, and then the forms
;;; are the inputs to the instruction.

;;; Generate code for the actual instruction building for a continuation.
;;; The builder must already be pointing to the right place.
(defun %build-continuation-body (builder body &optional extra)
  (let ((letbindings
          (loop for bind in (butlast body)
                do (assert (consp bind))
                when (and (consp bind) (eql (first bind) :=))
                  collect (let ((name (second bind))
                                (form (third bind))
                                (iname (or (fourth bind)
                                           `',(second bind))))
                            `(,name
                              (build ,builder (make-instruction
                                               ',(first form) ,iname
                                               ,@(rest form)))))
                else
                  collect `(,(gensym "IGNORED")
                            (build ,builder (make-instruction ',(first bind)
                                                              nil
                                                              ,@(rest bind))))))
        (term (first (last body))))
    `(let* (,@letbindings)
       (declare (ignorable ,@(mapcar #'first letbindings)))
       (build ,builder (make-terminator ',(first term) nil ,@(rest term)))
       ,@extra)))

(defun %namespec (namespec)
  (if (consp namespec)
      (values (first namespec) (second namespec))
      (values namespec `',namespec)))

(defmacro %assemble-continuation (cform (&rest children) &body body)
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
                                     gchildren asmbody)))))))
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
       ,@(loop for (rcname) in childinfo collect `(add-child ,cform ,rcname))
       ;; Build this continuation
       (build-continuation *builder* ,cform)
       ,(%build-continuation-body '*builder* body))))

(defmacro assemble-continuation (name (paramname) (&rest children)
                                 &body body)
  (multiple-value-bind (name nameform) (%namespec name)
    (multiple-value-bind (pname pnameform) (%namespec paramname)
      `(with-builder ()
         (let* ((,name (make-continuation ,nameform ,pnameform))
                (,pname (parameter ,name)))
           (%assemble-continuation ,name ,children ,@body)
           ,name)))))

#+(or)
(defmacro with-continuation-assembly (((&whole spec
                                               name paramname parent
                                               &optional ename pname)
                                       &body asmbody)
                                      &body body)
  (declare (ignore name paramname parent ename pname))
  (let ((bsym (gensym "BUILDER")))
    `(let ((,bsym (make-instance 'builder)))
       (%with-continuation ,spec (,bsym)
         ,(%assemble-continuation bsym asmbody body)))))

(defmacro assemble ((name enclosedname retname) &body (cont))
  (let ((gstart (gensym "START")))
    (multiple-value-bind (name nameform) (%namespec name)
      (multiple-value-bind (ename enameform) (%namespec enclosedname)
        (multiple-value-bind (rname rnameform) (%namespec retname)
          `(let* ((,ename (make-instance 'enclosed :name ,enameform))
                  (,rname
                    (make-continuation ,rnameform (gensym "RETURN-VALUE")))
                  (,name (make-instance 'function
                           :name ,nameform :enclosed ,ename :rcont ,rname))
                  (,gstart (assemble-continuation ,@cont)))
             (setf (%parent ,gstart) ,name (%start ,name) ,gstart
                   (%parent ,rname) ,gstart)
             ,name))))))
