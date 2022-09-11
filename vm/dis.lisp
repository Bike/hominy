(in-package #:burke/vm/asm)

(defun instruction-length (op)
  (ecase op
    ((o:nop o:drop o:dup o:make-cell o:cell-ref o:cell-set o:cons o:car o:cdr o:return
            o:combine o:tail-combine o:lookup o:unwrap o:wrap
            o:err-if-not-cons o:err-if-not-null o:err-if-not-bool)
     1)
    ((o:ref o:set o:closure o:const o:arg o:listify-args
            o:check-arg-count-= o:check-arg-count->= o:list o:jump o:jump-if-true
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
                o:combine o:tail-combine o:lookup o:unwrap o:wrap
                o:err-if-not-cons o:err-if-not-null o:err-if-not-bool)
         (fixed 0))
        ((o:ref o:set o:closure o:const o:arg o:listify-args o:list o:jump o:jump-if-true
                o:check-arg-count-= o:check-arg-count->=
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
                        :start (vm:gep obj)
                        :end (vm:end obj)))

(defmethod disassemble ((obj burke/interpreter:applicative))
  (disassemble (burke/interpreter:unwrap obj)))

(defun module ()
  (burke/interpreter:make-fixed-environment
   '(burke/interpreter/syms::disassemble)
   (list (burke/interpreter:wrap
          (burke/interpreter:make-builtin-operative
           (lambda (env frame combinand)
             (declare (ignore env frame))
             (destructuring-bind (combiner) combinand
               (disassemble combiner))))))))
