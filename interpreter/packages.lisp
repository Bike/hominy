;;; This environment is where symbols within the runtime live.
;;; It's its own package so that other code can define their own symbols and bindings.
(defpackage #:burke/interpreter/syms
  (:use))

(defpackage #:burke/interpreter
  (:use #:cl)
  (:local-nicknames (#:syms #:burke/interpreter/syms))
  (:shadow #:eval #:boolean #:variable #:ignore)
  (:export #:eval #:combine #:evalseq)
  (:export #:combiner #:operative #:applicative #:wrap #:unwrap
           #:make-builtin-operative #:make-derived-operative)
  (:export #:name)
  (:export #:macro #:make-macro)
  (:export #:ignore #:inert #:boolean #:true #:false)
  (:export #:environment #:define #:lookup #:make-environment #:make-fixed-environment
           #:make-uninitialized-fixed-environment #:initialize-fixed-environment)
  (:export #:*base*)
  (:export #:bind-ptree-to-vector)
  ;; Derived operative internals, for use with compilers
  ;; TODO: rename env to static-env, probably
  (:export #:derived-operative #:ptree #:eparam #:env #:body)
  ;; Serialization
  (:export #:marshal #:marshal-to-file #:unmarshal #:unmarshal-from-file))
