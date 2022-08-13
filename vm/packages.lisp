(defpackage #:burke/vm
  (:use #:cl)
  (:shadow #:disassemble)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:o #:burke/vm/ops)))

;;; BURKE/VM/OPS specially defined in ops.lisp
