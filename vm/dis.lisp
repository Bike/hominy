(in-package #:burke/vm/asm)

(defun instruction-length (op)
  (ecase op
    ((o:nop o:drop o:dup o:cons o:car o:cdr o:return
            o:combine o:tail-combine o:lookup o:unwrap
            o:err-if-not-cons o:err-if-not-null)
     1)
    ((o:ref o:set o:closure o:const o:list o:jump o:jump-if-true
            o:enclose o:make-environment o:call o:tail-call)
     2)))

(defun disassemble-instruction (bytecode ip)
  (let* ((byte (aref bytecode ip))
         (pair (rassoc byte o:*ops*)))
    (unless pair (error "Unknown opcode ~d" byte))
    (flet ((fixed (n)
             (prog1
                 (cons (car pair)
                       (loop repeat n collect (aref bytecode (incf ip))))
               (incf ip))))
      (ecase (car pair)
        ((o:nop o:drop o:dup o:cons o:car o:cdr o:return
                o:combine o:tail-combine o:lookup o:unwrap
                o:err-if-not-cons o:err-if-not-null)
         (fixed 0))
        ((o:ref o:set o:closure o:const o:list o:jump o:jump-if-true
                o:enclose o:make-environment o:call o:tail-call)
         (fixed 1))))))

(defun disassemble-bytecode (bytecode &key (start 0))
  (loop with bytecode-len = (length bytecode)
        with pc = start
        for op = (car (rassoc (aref bytecode pc) o:*ops*))
        collect (disassemble-instruction bytecode pc)
        do (incf pc (instruction-length op))
        until (>= pc bytecode-len)))

(defgeneric disassemble (object))

(defmethod disassemble ((obj vm:module))
  (disassemble-bytecode (burke/vm:bytecode obj)))

;; can't do CODE until it has an end point - oops!
