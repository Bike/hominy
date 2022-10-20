(in-package #:burke/vm/asm)

(defun disassemble-instruction (bytecode ip)
  (let* ((byte (aref bytecode ip))
         (info (burke/vm:decode byte)))
    (unless info (error "Unknown opcode ~d" byte))
    (cons (car info)
          (loop for i from 0 below (third info)
                for argspec = (nth i (cdddr info))
                for byte = (aref bytecode (+ ip i 1))
                collect (if (eq argspec 'o:jump) ; signed
                            (if (logbitp 7 byte) (- byte #x100) byte)
                            byte)))))

;;; Reconstruct a list of labels from raw bytecode by looking at jump destinations.
;;; TODO: In the future, it may be worthwhile to save annotations from the
;;; assembler. Then we could for example give real names to labels, or use them
;;; more easily to reconstruct compiler IR.
(defun collect-labels (bytecode &key (start 0) (end (length bytecode)))
  (declare (optimize debug))
  (loop with pc = start
        with labels = '()
        for (name byte size . args) = (burke/vm:decode (aref bytecode pc))
        do (loop for i from 0 below size
                 for arg = (nth i args)
                 for raw = (aref bytecode (+ pc i 1))
                 when (eq arg 'o:jump) ; label
                   do (let* (;; resolve the label (it's a signed byte, for now)
                             (rel (if (logbitp 7 raw) (- raw #x100) raw))
                             (abs (+ pc rel)))
                        (unless (member abs labels) ; already got this label
                          (push abs labels))))
           (incf pc (1+ size))
        until (>= pc end)
        finally (return (loop for abs in (sort labels #'<)
                              for i from 0
                              collect (cons abs (format nil "L~d" i))))))

(defun disassemble-bytecode (bytecode &key (start 0) (end (length bytecode))
                                        constants closed)
  (format t "~&---disassembly---~%")
  (loop with pc = start
        with labels = (collect-labels bytecode :start start :end end)
        for inst-info = (burke/vm:decode (aref bytecode pc))
        for dis = (disassemble-instruction bytecode pc)
        for lab = (assoc pc labels)
        when lab
          do (format t "~&~a:" (cdr lab))
        do (format t "~& ~a~{ ~d~}" (first dis) (rest dis))
        when (some #'identity (cdddr inst-info)) ; special formatting
          do (format t " ; ")
             (loop for arg in (rest dis)
                   for spec in (cdddr inst-info)
                   do (ecase spec
                        ((o:const)
                         (when constants (format t "~s " (aref constants arg))))
                        ((o:jump)
                         (let ((lab (assoc (+ pc arg) labels)))
                           (when lab (format t " ; ~a" (cdr lab)))))
                        ((o:closure)
                         (when closed (format t " ; ~a" (aref closed arg))))
                        ((nil))))
        do (incf pc (1+ (third inst-info)))
        until (>= pc end)))

(defgeneric disassemble (object))

(defmethod disassemble ((obj vm:module))
  ;; TODO: Print closure variables from each function individually.
  ;; (And, really, break up the disassembly into functions.)
  (disassemble-bytecode (burke/vm:bytecode obj)
                        :constants (burke/vm:constants obj)))

(defmethod disassemble ((obj vm:closure)) (disassemble (vm:code obj)))

(defmethod disassemble ((obj vm:code))
  (let ((mod (burke/vm:module obj)))
    (disassemble-bytecode (burke/vm:bytecode mod)
                          :constants (burke/vm:constants mod)
                          :closed (burke/vm:closed obj)
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
