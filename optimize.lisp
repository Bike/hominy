(in-package #:burke)

(defun constant-type-p (type)
  (and (typep type 'type:member)
       (= (length (type:elements type)) 1)))

(defun constant-type-value (type)
  (first (type:elements type)))

(defun constant-propagate-function (function)
  (let ((module (ir:module function)))
    (ir:map-instructions
     (lambda (inst)
       (loop for use in (ir:uinputs inst)
             for info = (ir:info use)
             for type = (flow:type info)
             when (and (constant-type-p type)
                       (not (typep (ir:definition use) 'ir:constant)))
               do (setf (ir:definition use)
                        (ir:constant (constant-type-value type) module))))
     function)))

(defun constant-propagate (module)
  (ir:map-functions #'constant-propagate-function module))

(defgeneric maybe-replace-instruction (instruction)
  (:method ((inst ir:instruction))))

(defmethod maybe-replace-instruction ((inst ir:sequence))
  (let* ((uins (ir:uinputs inst))
         (continuation (ir:definition (first uins)))
         (env (ir:definition (third uins)))
         (formsu (second uins))
         (forms (ir:definition formsu))
         (formsinfo (ir:info formsu))
         (formstype (flow:type formsinfo))
         (null (type:member nil))
         (top (type:top))
         (list1 (type:cons top null)))
    (cond ((type:subtypep formstype list1)
           ;; ($sequence foo)
           (ir:replace-terminator inst
               (ir:eval continuation (ir:car forms) env))
           t)
          ((type:subtypep formstype null)
           ;; ($sequence)
           (ir:replace-terminator inst
               (ir:continue continuation 'inert))
           t)
          (t nil))))

(defmethod maybe-replace-instruction ((inst ir:eval))
  (let* ((uins (ir:uinputs inst))
         (continuation (ir:definition (first uins)))
         (env (ir:definition (third uins)))
         (formu (second uins))
         (form (ir:definition formu))
         (forminfo (ir:info formu))
         (formtype (flow:type forminfo))
         (symbol (type:symbol))
         (top (type:top))
         (cons (type:cons top top)))
    ;; TODO: Self evaluating objects
    (cond ((type:subtypep formtype symbol)
           (ir:replace-terminator inst
               (ir:continue continuation (ir:lookup form env)))
           t)
          ((type:subtypep formtype cons)
           (let* ((evalcont (ir:continuation inst))
                  (combcont
                    (ir:assemble-continuation combine (combiner) evalcont
                        ()
                      (ir:combination continuation combiner
                                      (ir:cons (ir:cdr form) env)))))
             (ir:replace-terminator inst
                 (ir:eval combcont (ir:car form) env))
             t))
          (t nil))))

(defun optimize-function (function enclosed-info)
  (tagbody
   loop
     (flow:forward-propagate-datum (ir:enclosed function) enclosed-info)
     (ir:map-instructions
      (lambda (inst)
        (when (maybe-replace-instruction inst)
          (go loop)))
      function))
  ;; constant "propagation" never actually adds information, as the actual
  ;; propagation is done by flow. so we do this last.
  (constant-propagate-function function)
  (values))
