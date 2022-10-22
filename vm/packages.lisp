(defpackage #:hominy/vm/class
  (:use)
  (:export #:cell #:cons #:boolean #:null #:applicative))

(defpackage #:hominy/vm
  (:use #:cl)
  (:local-nicknames (#:i #:hominy/interpreter)
                    (#:o #:hominy/vm/ops)
                    (#:class #:hominy/vm/class))
  (:export #:decode #:instruction-length)
  (:export #:module #:bytecode #:constants
           #:code #:closure #:enclose #:closed)
  ;; needed for disassemble
  (:export #:gep #:end))

(defpackage #:hominy/vm/asm
  (:use #:cl)
  (:shadow #:disassemble)
  (:local-nicknames (#:vm #:hominy/vm)
                    (#:o #:hominy/vm/ops))
  (:export #:assemble #:link #:make-label #:emit-label)
  (:export #:cmodule #:constant-index #:closure-index
           #:cfunction #:gep #:cep #:lep #:nlocals #:nstack #:closed #:nbytes)
  (:export #:disassemble #:disassemble-bytecode)
  (:export #:module))

;;; BURKE/VM/OPS specially defined in ops.lisp
