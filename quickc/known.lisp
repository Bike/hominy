(in-package #:burke/quickc)

;;; FIXME: At least use a hash table, cripes.
(defun compile-known-operation (name combinand cenv context)
  (ecase name
    ((syms::$vau) (compile-$vau combinand cenv context))))

(defun plist-p (object)
  (typecase object
    ((or symbol null i:ignore) t)
    (cons (and (plist-p (car object)) (plist-p (cdr object))))
    (t nil)))

;;; These all work on constants for the time being, to preserve my sanity.
(defun compile-$vau (combinand cenv context)
  (destructuring-bind (plist eparam . body) combinand
    (unless (plist-p plist)
      (warn "Bad syntax: $VAU given a non-plist ~a" plist)
      (return-from compile-$vau nil))
    (unless (typep eparam '(or symbol i:ignore))
      (warn "Bad syntax: $VAU given a bad eparam ~a" eparam)
      (return-from compile-$vau nil))
    ;; We don't need the combiner, drop it.
    ;; Obviously it would have been better to not bother compiling a reference,
    ;; but this compiler is not very good and does not have IR structures
    ;; to facilitate that.
    (assemble context 'o:drop)
    ;; $VAU has no side effects, so outside of a value context do nothing.
    (unless (valuep context)
      (return-from compile-$vau (result (info:default-info) 0 0)))
    (let* ((cmod (asm:cmodule (cfunction context)))
           (res (compile-operative plist eparam body cenv cmod))
           (info (info res))
           (cf (info:data info))
           (cfi (asm:constant-index cf cmod)))
      (cond ((zerop (length (asm:closed cf)))
             ;; not a closure, so just use a constant.
             ;; (With quickc this will never happen in practice, I think.)
             (assemble context 'o:const cfi)
             (when (tailp context) (assemble context 'o:return))
             (result info 0 1))
            (t ; making a closure. push closed-over vals to the stack, enclose.
             (let ((nstack 0))
               (loop for c across (asm:closed cf)
                     do (cond ((eq c cenv) ; entire current environment
                               (assemble context 'o:ref (env-index context))
                               (incf nstack 1))
                              ((symbolp c) ; look something up in current env
                               (incf nstack
                                     (nstack (compile-symbol c cenv
                                                             (context context
                                                                      :valuep t
                                                                      :tailp nil)))))
                              (t (error "?How"))))
               (assemble context 'o:enclose cfi)
               (when (tailp context) (assemble context 'o:return))
               (result info 0 nstack)))))))
