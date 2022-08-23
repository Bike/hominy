(defpackage #:burke/vm
  (:use #:cl)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:o #:burke/vm/ops))
  (:export #:module #:bytecode #:code #:closure #:enclose))

(defpackage #:burke/vm/asm
  (:use #:cl)
  (:shadow #:disassemble)
  (:local-nicknames (#:vm #:burke/vm)
                    (#:o #:burke/vm/ops))
  (:export #:assemble #:link #:make-label #:emit-label)
  (:export #:cmodule #:constant-index #:closure-index
           #:cfunction #:sep #:nlocals #:nstack #:closed #:nbytes)
  (:export #:disassemble #:disassemble-bytecode))

;;; BURKE/VM/OPS specially defined in ops.lisp
