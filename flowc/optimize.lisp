(in-package #:hominy)

(defun constant-type-p (type)
  (and (typep type 'hominy/type:member)
       (= (length (hominy/type:elements type)) 1)))

(defun constant-type-value (type)
  (first (hominy/type:elements type)))

(defun constant-propagate-function (function)
  (let ((module (hominy/ir:module function)))
    (hominy/ir:map-instructions
     (lambda (inst)
       (loop for use in (hominy/ir:uinputs inst)
             for info = (hominy/ir:info use)
             for type = (hominy/info:type info)
             when (and (constant-type-p type)
                       (not (typep (hominy/ir:definition use) 'hominy/ir:constant)))
               do (setf (hominy/ir:definition use)
                        (hominy/ir:constant (constant-type-value type) module))))
     function)))

(defun constant-propagate (module)
  (hominy/ir:map-functions #'constant-propagate-function module))

(defgeneric maybe-replace-instruction (instruction)
  (:method ((inst hominy/ir:instruction))))

(defmethod maybe-replace-instruction ((inst hominy/ir:sequence))
  (let* ((uins (hominy/ir:uinputs inst))
         (continuation (hominy/ir:definition (first uins)))
         (env (hominy/ir:definition (third uins)))
         (formsu (second uins))
         (forms (hominy/ir:definition formsu))
         (formsinfo (hominy/ir:info formsu))
         (formstype (hominy/info:type formsinfo))
         (null (hominy/type:member nil))
         (top (hominy/type:top))
         (list1 (hominy/type:cons top null)))
    (cond ((hominy/type:subtypep formstype list1)
           ;; ($sequence foo)
           (hominy/ir:replace-terminator inst
               (hominy/ir:eval continuation (hominy/ir:car forms) env))
           t)
          ((hominy/type:subtypep formstype null)
           ;; ($sequence)
           (hominy/ir:replace-terminator inst
               (hominy/ir:continue continuation 'i:inert))
           t)
          (t nil))))

(defmethod maybe-replace-instruction ((inst hominy/ir:eval))
  (let* ((uins (hominy/ir:uinputs inst))
         (continuation (hominy/ir:definition (first uins)))
         (env (hominy/ir:definition (third uins)))
         (formu (second uins))
         (form (hominy/ir:definition formu))
         (forminfo (hominy/ir:info formu))
         (formtype (hominy/info:type forminfo))
         (symbol (hominy/type:symbol))
         (top (hominy/type:top))
         (cons (hominy/type:cons top top)))
    ;; TODO: Self evaluating objects
    (cond ((hominy/type:subtypep formtype symbol)
           (hominy/ir:replace-terminator inst
               (hominy/ir:continue continuation (hominy/ir:lookup form env)))
           t)
          ((hominy/type:subtypep formtype cons)
           (let* ((evalcont (hominy/ir:continuation inst))
                  (combcont
                    (hominy/ir:assemble-continuation combine (combiner) evalcont
                        ()
                      (hominy/ir:combination continuation combiner
                                      (hominy/ir:cons (hominy/ir:cdr form) env)))))
             (hominy/ir:replace-terminator inst
                 (hominy/ir:eval combcont (hominy/ir:car form) env))
             t))
          (t nil))))

(defun optimize-function (function enclosed-info)
  (tagbody
   loop
     (hominy/flow:forward-propagate-datum (hominy/ir:enclosed function) enclosed-info)
     (hominy/ir:map-instructions
      (lambda (inst)
        (when (maybe-replace-instruction inst)
          (go loop)))
      function))
  ;; constant "propagation" never actually adds information, as the actual
  ;; propagation is done by flow. so we do this last.
  (constant-propagate-function function)
  (values))
