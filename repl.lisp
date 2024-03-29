(in-package #:hominy)

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

(defvar *hominy-readtable*
  (let ((rt (copy-readtable nil)))
    (install-reader-macros rt)
    rt))

(locally
    #+sbcl
  (declare (sb-ext:muffle-conditions style-warning)) ; &optional and &key complaint
  (defun read-from-string (string &optional (eof-error-p t) eof-value
                           &key (start 0) end preserve-whitespace)
    (let ((*readtable* *hominy-readtable*)
          (*package* (find-package "BURKE/INTERPRETER/SYMS")))
      (cl:read-from-string string eof-error-p eof-value
                           :start start :end end
                           :preserve-whitespace preserve-whitespace))))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursivep)
  (let ((*readtable* *hominy-readtable*)
        (*package* (find-package "BURKE/INTERPRETER/SYMS")))
    (cl:read stream eof-error-p eof-value recursivep)))

(defun repl (&key (modules nil))
  (let* ((*readtable* *hominy-readtable*)
         (*package* (find-package "BURKE/INTERPRETER/SYMS"))
         (repl-env (apply #'i:make-environment baselib:*base* modules)))
    ;; KLUDGE
    (i:define baselib:*basec* 'hominy/interpreter/syms::compilation-environment
      repl-env)
    (catch 'abort
      (with-simple-restart (abort "Abort the Hominy REPL.")
        (loop (with-simple-restart (continue "Return to the Hominy REPL.")
                (format t "~&> ")
                (format t "~&~:a~%" (i:eval (read) repl-env))))))))
