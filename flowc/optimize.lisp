(in-package #:burke)

(defun constant-type-p (type)
  (and (typep type 'burke/type:member)
       (= (length (burke/type:elements type)) 1)))

(defun constant-type-value (type)
  (first (burke/type:elements type)))

(defun constant-propagate-function (function)
  (let ((module (burke/ir:module function)))
    (burke/ir:map-instructions
     (lambda (inst)
       (loop for use in (burke/ir:uinputs inst)
             for info = (burke/ir:info use)
             for type = (burke/info:type info)
             when (and (constant-type-p type)
                       (not (typep (burke/ir:definition use) 'burke/ir:constant)))
               do (setf (burke/ir:definition use)
                        (burke/ir:constant (constant-type-value type) module))))
     function)))

(defun constant-propagate (module)
  (burke/ir:map-functions #'constant-propagate-function module))

(defgeneric maybe-replace-instruction (instruction)
  (:method ((inst burke/ir:instruction))))

(defmethod maybe-replace-instruction ((inst burke/ir:sequence))
  (let* ((uins (burke/ir:uinputs inst))
         (continuation (burke/ir:definition (first uins)))
         (env (burke/ir:definition (third uins)))
         (formsu (second uins))
         (forms (burke/ir:definition formsu))
         (formsinfo (burke/ir:info formsu))
         (formstype (burke/info:type formsinfo))
         (null (burke/type:member nil))
         (top (burke/type:top))
         (list1 (burke/type:cons top null)))
    (cond ((burke/type:subtypep formstype list1)
           ;; ($sequence foo)
           (burke/ir:replace-terminator inst
               (burke/ir:eval continuation (burke/ir:car forms) env))
           t)
          ((burke/type:subtypep formstype null)
           ;; ($sequence)
           (burke/ir:replace-terminator inst
               (burke/ir:continue continuation 'i:inert))
           t)
          (t nil))))

(defmethod maybe-replace-instruction ((inst burke/ir:eval))
  (let* ((uins (burke/ir:uinputs inst))
         (continuation (burke/ir:definition (first uins)))
         (env (burke/ir:definition (third uins)))
         (formu (second uins))
         (form (burke/ir:definition formu))
         (forminfo (burke/ir:info formu))
         (formtype (burke/info:type forminfo))
         (symbol (burke/type:symbol))
         (top (burke/type:top))
         (cons (burke/type:cons top top)))
    ;; TODO: Self evaluating objects
    (cond ((burke/type:subtypep formtype symbol)
           (burke/ir:replace-terminator inst
               (burke/ir:continue continuation (burke/ir:lookup form env)))
           t)
          ((burke/type:subtypep formtype cons)
           (let* ((evalcont (burke/ir:continuation inst))
                  (combcont
                    (burke/ir:assemble-continuation combine (combiner) evalcont
                        ()
                      (burke/ir:combination continuation combiner
                                      (burke/ir:cons (burke/ir:cdr form) env)))))
             (burke/ir:replace-terminator inst
                 (burke/ir:eval combcont (burke/ir:car form) env))
             t))
          (t nil))))

(defun optimize-function (function enclosed-info)
  (tagbody
   loop
     (burke/flow:forward-propagate-datum (burke/ir:enclosed function) enclosed-info)
     (burke/ir:map-instructions
      (lambda (inst)
        (when (maybe-replace-instruction inst)
          (go loop)))
      function))
  ;; constant "propagation" never actually adds information, as the actual
  ;; propagation is done by flow. so we do this last.
  (constant-propagate-function function)
  (values))
