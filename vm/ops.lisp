(cl:in-package #:burke)

(macrolet ((def (&rest ops)
             (let ((map (loop for (op len . argspec) in ops for i from 0
                              collect (list* (symbol-name op) i len
                                             (mapcar #'symbol-name argspec)))))
               `(progn
                  (defpackage #:burke/vm/ops
                    (:use)
                    (:export "*OPS*")
                    (:export ,@(mapcar #'car map)))
                  (in-package #:burke/vm/ops)
                  ;; Expansion delayed until the package is actually defined
                  (dmap ,map))))
           (dmap (map)
             (flet ((osym (name) (find-symbol name "BURKE/VM/OPS")))
               (let ((map (loop for (name id len . args) in map
                                for sym = (osym name)
                                for argspecs = (mapcar #'osym args)
                                collect (list* sym id len argspecs))))
                 `(progn
                    (defparameter ,(find-symbol "*OPS*" "BURKE/VM/OPS") ',map)
                    ,@(loop for (sym i) in map
                            collect `(defconstant ,sym ,i)))))))
  ;; Format is (name instruction-length argspecs...)
  ;; an argspec can be NIL, CONST, JUMP, or CLOSURE, and is a hint to the disassembler
  ;; on how to format the value. CONST means it's an index into the constants,
  ;; JUMP means it's a relative position, and CLOSURE a closure index.
  ;; NIL, the default, means the disassembler should do nothing special.
  (def
    ;; stack
    (nop 0) (drop 0) (dup 0)
    ;; variables
    (ref 1) (set 1) (closure 1 closure) (const 1 const)
    ;; arguments
    (arg 1) (listify-args 1) (check-arg-count-= 1) (check-arg-count->= 1)
    ;; control
    (return 0) (jump 1 jump) (jump-if-true 1 jump)
    ;; objects
    (construct 1 const) (check-class 1) (slot-read 2 const) (slot-write 2 const)
    ;; functions/calls
    (enclose 1 const) (make-environment 1 const) ; rethink?
    (call 1) (tail-call 1)))
