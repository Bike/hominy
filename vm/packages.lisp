(defpackage #:burke/vm/class
  (:use)
  (:export #:cell #:cons #:boolean #:null #:applicative))

(defpackage #:burke/vm
  (:use #:cl)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:o #:burke/vm/ops)
                    (#:class #:burke/vm/class))
  (:export #:decode #:instruction-length)
  (:export #:module #:bytecode #:constants
           #:code #:closure #:enclose #:closed)
  ;; needed for disassemble
  (:export #:gep #:end))

(defpackage #:burke/vm/asm
  (:use #:cl)
  (:shadow #:disassemble)
  (:local-nicknames (#:vm #:burke/vm)
                    (#:o #:burke/vm/ops))
  (:export #:assemble #:link #:make-label #:emit-label)
  (:export #:cmodule #:constant-index #:closure-index
           #:cfunction #:gep #:cep #:lep #:nlocals #:nstack #:closed #:nbytes)
  (:export #:disassemble #:disassemble-bytecode)
  (:export #:module))

;;; BURKE/VM/OPS specially defined in ops.lisp
