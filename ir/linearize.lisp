(in-package #:burke/ir)

;;;; NOTE: Won't work if there are circular dependencies.

;;; Ensure all inputs of this instruction (and their inputs, etc.) are
;;; linearized. PREV is what the %PREV of the head of the subsequence
;;; originating from this instruction ought to be set to.
(defun linearize-instruction (instruction prev)
  ;; We go through the inputs and linearize anything that hasn't been yet.
  ;; The input we last managed to linearize is the new prev of this
  ;; instruction, or if we didn't linearize any, the new prev is the argument.
  ;; Example: say we have A>B>C A>D>C E>C. We start with
  ;; (linearize-instruction C nil). This checks the first input B and finds it
  ;; has not been linearized, so we do (linearize-instruction B nil).
  ;; This similarly checks its one input A, so (linearize-instruction A nil).
  ;; A has no inputs, so its prev is set to the argument (NIL).
  ;; B last linearized A, so its prev is set to A.
  ;; Back to linearizing C, we now have prev=B, and do
  ;; (linearize-instruction D B). This checks its input A, but it's already
  ;; linearized, so D's prev is set to the argument B instead.
  ;; Back to linearizing C, prev=D, and we (linearize-instruction E D).
  ;; E has no inputs, so its prev is set to the argument D.
  ;; Finally, C's prev is set to E, so we have ABDEC.
  (dolist (input (inputs instruction))
    (when (and (typep input 'instruction)
               (null (%next input))) ; not yet linearized
      (linearize-instruction input prev)
      (setf prev input)))
  (cond ((null prev))
        ((null (%next prev))
         (setf (%next prev) instruction))
        (t (error "BUG: %NEXT of %PREV already set! ~a" instruction)))
  (setf (%prev instruction) prev)
  (values))

(defun linearize-continuation (continuation prev-continuation)
  (linearize-instruction (terminator continuation)
                         (if prev-continuation
                             (terminator prev-continuation)
                             nil)))

;;; Find the start of the linearization, i.e. the instruction with %PREV=NIL.
;;; This could be done more efficiently by saving it, probably.
(defun find-start (instruction)
  (loop for inst = instruction then prev
        for prev = (%prev inst)
        until (null prev)
        finally (return inst)))
;;; Ditto with the end.
(defun find-end (instruction)
  (loop for inst = instruction then next
        for next = (%next inst)
        until (null next)
        finally (return inst)))

(defun linearize-function (function)
  (let ((prev-cont nil))
    (map-continuations (lambda (cont)
                         (linearize-continuation cont prev-cont)
                         (setf prev-cont cont))
                       function))
  (let ((term (terminator (start function))))
    (setf (%start-inst function) (find-start term)
          (%end-inst function) (find-end term)))
  (values))

;;; Clear the %prev and %next of all instructions (and the function) so that
;;; it can be re-linearized. Should be done after graph modifications.
;;; Note that it should be OK to run this function even if some instructions
;;; previously part of the linearization have been removed from control flow,
;;; and in fact it's a good idea to do so to avoid a memory leak as dead
;;; instructions are kept "alive" through the linearization.
(defun clear-linearization (function)
  (loop for inst = (%start-inst function) then next
        for next = (%next inst)
        while next
        do (setf (%prev inst) nil (%next inst) nil))
  (values))

(defun relinearize-function (function)
  (clear-linearization function)
  (linearize-function function)
  (values))
