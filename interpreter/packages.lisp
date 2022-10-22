;;; This environment is where symbols within the runtime live.
;;; It's its own package so that other code can define their own symbols and bindings.
(defpackage #:hominy/interpreter/syms
  (:use))

(defpackage #:hominy/interpreter
  (:use #:cl)
  (:local-nicknames (#:syms #:hominy/interpreter/syms))
  (:shadow #:eval #:boolean #:variable #:ignore #:throw #:continue)
  (:export #:eval #:combine #:call #:evalseq)
  (:export #:combiner #:operative #:applicative #:wrap #:unwrap
           #:make-builtin-operative)
  (:export #:name)
  (:export #:ignore #:inert #:boolean #:true #:false)
  (:export #:frame #:frame-parent #:continue)
  (:export #:environment #:define #:lookup #:map-parents #:map-bindings #:local-cell
           #:make-environment #:make-fixed-environment
           #:make-fixed-environment-with-cells
           #:make-uninitialized-fixed-environment #:initialize-fixed-environment
           #:make-immutable-environment #:copy-env-immutable)
  (:export #:make-cell #:cellp #:cell-value)
  (:export #:user-class #:nslots
           #:construct-user-object #:of-user-class-p #:slot-access)
  (:export #:ptree-augmenter #:bind-ptree #:bind-ptree-to-vector)
  ;; Serialization
  (:export #:marshal #:marshal-to-file #:unmarshal #:unmarshal-from-file))
