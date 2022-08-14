(in-package #:burke/vm/asm)

(defun disassemble-bytecode (bytecode &key (start 0))
  (loop with pc = start
        with bytecode-len = (length bytecode)
        for byte = (aref bytecode pc)
        for pair = (rassoc byte o:*ops*)
        unless pair
          do (error "Unknown opcode ~d" byte)
        collect (flet ((fixed (n)
                         (prog1
                             (cons (car pair)
                                   (loop repeat n collect (aref bytecode (incf pc))))
                           (incf pc))))
                  (ecase (car pair)
                    ((o:nop o:drop o:cons o:car o:cdr o:return
                            o:combine o:tail-combine o:lookup
                            o:err-if-not-cons o:err-if-not-null)
                     (fixed 0))
                    ((o:ref o:set o:closure o:const o:list o:jump o:jump-if-true
                            o:enclose o:make-environment o:call o:tail-call)
                     (fixed 1))))
        until (>= pc bytecode-len)))

(defgeneric disassemble (object))

(defmethod disassemble ((obj vm:module))
  (disassemble-bytecode (bytecode obj)))

;; can't do CODE until it has an end point - oops!
