(in-package #:burke)

(defun read-\#i (stream dispchar num)
  (declare (cl:ignore dispchar num))
  (flet ((check-char (char)
           (let ((c (read-char stream)))
             (unless (char= c char)
               (error "#i misspelling")))))
    (let ((next (read-char stream)))
      (ecase next
        ((#\g) (map nil #'check-char "nore") i:ignore)
        ((#\n) (map nil #'check-char "ert") i:inert)))))

(defun read-#t (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  i:true)

(defun read-#f (stream dispchar num)
  (declare (cl:ignore stream dispchar num))
  i:false)

(defun install-reader-macros (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\i #'read-#i readtable)
  (set-dispatch-macro-character #\# #\t #'read-#t readtable)
  (set-dispatch-macro-character #\# #\f #'read-#f readtable)
  (values))

(defvar *burke-readtable*
  (let ((rt (copy-readtable nil)))
    (install-reader-macros rt)
    rt))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                         &key (start 0) end preserve-whitespace)
  (let ((*readtable* *burke-readtable*)
        (*package* (find-package "BURKE/INTERPRETER/SYMS")))
    (cl:read-from-string string eof-error-p eof-value
                         :start start :end end
                         :preserve-whitespace preserve-whitespace)))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursivep)
  (let ((*readtable* *burke-readtable*)
        (*package* (find-package "BURKE/INTERPRETER/SYMS")))
    (cl:read stream eof-error-p eof-value recursivep)))

(defun repl (&key (modules nil))
  (let* ((*readtable* *burke-readtable*)
         (*package* (find-package "BURKE/INTERPRETER/SYMS"))
         (repl-env (apply #'i:make-environment (i:make-standard-environment) modules)))
    (catch 'abort
      (with-simple-restart (abort "Abort the Burke REPL.")
        (loop (with-simple-restart (continue "Return to the Burke REPL.")
                (format t "~&> ")
                (format t "~&~:a~%" (i:eval (read) repl-env))))))))
