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

(defun maybe-replace-seq (inst)
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
                                  (ir:eval continuation (ir:car forms) env)))
          ((type:subtypep formstype null)
           ;; ($sequence)
           (let ((continuation (first (ir:inputs inst))))
             (ir:replace-terminator inst
                                    (ir:continue continuation 'inert)))))))

(defun replace-seqs-function (function)
  (ir:map-instructions
   (lambda (inst)
     (when (typep inst 'ir:sequence)
       (maybe-replace-seq inst)))
   function))

(defun replace-seqs (module) (ir:map-functions #'replace-seqs-function module))
