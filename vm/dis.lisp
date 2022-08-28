(in-package #:burke/vm/asm)

(defun instruction-length (op)
  (ecase op
    ((o:nop o:drop o:dup o:make-cell o:cell-ref o:cell-set o:cons o:car o:cdr o:return
            o:combine o:tail-combine o:lookup o:unwrap
            o:err-if-not-cons o:err-if-not-null o:err-if-not-bool)
     1)
    ((o:ref o:set o:closure o:const o:arg o:listify-args o:list o:jump o:jump-if-true
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
        ((o:nop o:drop o:dup o:make-cell o:cell-ref o:cell-set o:cons o:car o:cdr o:return
                o:combine o:tail-combine o:lookup o:unwrap
                o:err-if-not-cons o:err-if-not-null o:err-if-not-bool)
         (fixed 0))
        ((o:ref o:set o:closure o:const o:arg o:listify-args o:list o:jump o:jump-if-true
                o:enclose o:make-environment o:call o:tail-call)
         (fixed 1))))))

(defun disassemble-bytecode (bytecode &key (start 0) (end (length bytecode)))
  (loop with pc = start
        for op = (car (rassoc (aref bytecode pc) o:*ops*))
        collect (disassemble-instruction bytecode pc)
        do (incf pc (instruction-length op))
        until (>= pc end)))

(defgeneric disassemble (object))

(defmethod disassemble ((obj vm:module))
  (disassemble-bytecode (burke/vm:bytecode obj)))

(defmethod disassemble ((obj vm:closure)) (disassemble (vm:code obj)))

(defmethod disassemble ((obj vm:code))
  (disassemble-bytecode (burke/vm:bytecode (burke/vm:module obj))
                        :start (vm:xep obj)
                        :end (vm:end obj)))
