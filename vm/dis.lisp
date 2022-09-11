(in-package #:burke/vm/asm)

(defun disassemble-instruction (bytecode ip)
  (let* ((byte (aref bytecode ip))
         (info (burke/vm:decode byte)))
    (unless info (error "Unknown opcode ~d" byte))
    (flet ((fixed (n)
             (prog1
                 (cons (car info)
                       (loop repeat n collect (aref bytecode (incf ip))))
               (incf ip))))
      (fixed (third info)))))

(defun disassemble-bytecode (bytecode &key (start 0) (end (length bytecode))
                                        constants)
  (format t "~&---disassembly---~%")
  (loop with pc = start
        for inst-info = (burke/vm:decode (aref bytecode pc))
        for dis = (disassemble-instruction bytecode pc)
        do (format t "~& ~a~{ ~d~}" (first dis) (rest dis))
           (when (and (eq (fourth inst-info) 'o:const) constants)
             (format t " ; ~a" (aref constants (second dis))))
           (incf pc (1+ (third inst-info)))
        until (>= pc end)))

(defgeneric disassemble (object))

(defmethod disassemble ((obj vm:module))
  (disassemble-bytecode (burke/vm:bytecode obj)
                        :constants (burke/vm:constants obj)))

(defmethod disassemble ((obj vm:closure)) (disassemble (vm:code obj)))

(defmethod disassemble ((obj vm:code))
  (let ((mod (burke/vm:module obj)))
    (disassemble-bytecode (burke/vm:bytecode mod)
                          :constants (burke/vm:constants mod)
                          :start (vm:gep obj)
                          :end (vm:end obj))))

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
               (disassemble combiner)
               burke/interpreter:inert)))))))
