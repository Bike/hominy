(in-package #:burke)

(defun read-\#i (stream dispchar num)
  (declare (cl:ignore dispchar num))
  (flet ((check-char (char)
           (let ((c (read-char stream)))
             (unless (char= c char)
               (error "#i misspelling")))))
    (let ((next (read-char stream)))
      (ecase next
        ((#\g) (map nil #'check-char "nore") (make-instance 'ignore))
        ((#\n) (map nil #'check-char "ert") (make-instance 'inert))))))

(defun read-#t (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  true)

(defun read-#f (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  false)

(defun install-reader-macros (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\i #'read-#i readtable)
  (set-dispatch-macro-character #\# #\t #'read-#t readtable)
  (set-dispatch-macro-character #\# #\f #'read-#f readtable)
  (values))

(defun repl ()
  (let* ((*readtable* (copy-readtable nil))
         (*package* (find-package "BURKE"))
         (ground (make-environment))
         (repl-env (make-environment ground)))
    (initialize-ground ground)
    (install-reader-macros)
    (catch 'abort
      (with-simple-restart (abort "Abort the Burke REPL.")
        (loop (with-simple-restart (continue "Return to the Burke REPL.")
                (format t "~&> ")
                (format t "~&~:a~%" (eval (read) repl-env))))))))
