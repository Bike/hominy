(in-package #:burke/ir)

(defgeneric verify (ir)
  (:method-combination progn))

(defmacro test (test thing datum &rest arguments)
  `(unless ,test
     (warn "Invalid ~s: ~@?" ,thing ,datum ,@arguments)))

(defmethod verify :around (ir)
  (handler-case (call-next-method)
    (serious-condition (e)
      (warn "Serious condition encountered while verifying ~a:~%~a"
            ir e))))

(defmethod verify progn ((ir datum))
  (test (and (slot-boundp ir '%name)
             (symbolp (name ir)))
        ir "datum's name is unbound or a non-symbol")
  (%map-uses (lambda (use)
               (test (eql (definition use) ir) ir
                     "datum's use ~a does not have it as a definition, but instead ~a"
                     use (definition use)))
             ir))

(defmethod verify progn ((ir user))
  (map nil (lambda (use)
             (test (eql (user use) ir) ir
                   "user's input use ~a does not have it as a user, but instead ~a"
                   use (user use)))
       (%uinputs ir)))

(defmethod verify progn ((ir parameter))
  (test (eql (parameter (continuation ir)) ir) ir
        "continuation ~a's parameter is ~a instead of this"
        (continuation ir) (parameter (continuation ir))))

(defmethod verify progn ((ir continuation))
  (unless (eql ir (rcont (function ir)))
    (let ((parent (parent ir)))
      (unless (typep parent 'function)
        (test (member ir (children parent)) ir
              "parent ~a does not list it as a child" parent)))
    (verify (parameter ir))
    #+(or) ; FIXME
    (map-instructions #'verify ir)
    (map-children #'verify ir)))

(defmethod verify progn ((ir function))
  (let ((modulep (slot-boundp ir '%module)))
    (test modulep ir "function's module slot is unbound")
    (when modulep
      (let* ((module (module ir))
             (modulep (typep module 'module)))
        (test modulep ir "function's module is not a module, but instead ~a"
              module)
        (when modulep
          (test (member ir (%functions module)) ir
                "function's module ~a does not record it" module)))))
  (let ((enclosedp (slot-boundp ir '%enclosed)))
    (test enclosedp ir "function's enclosed is not bound")
    (when enclosedp
      (test (typep (enclosed ir) 'enclosed)
            "function's enclosed is not an enclosed, but instead ~a"
            (enclosed ir))))
  (let ((startp (slot-boundp ir '%start)))
    (test startp ir "function's start is not bound")
    (when startp
      (test (typep (start ir) 'continuation) ir
            "function's start is not a continuation, but instead ~a"
            (start ir))))
  (let ((rcontp (slot-boundp ir '%rcont)))
    (test rcontp ir "function's return continuation is not bound")
    (when rcontp
      (test (typep (rcont ir) 'continuation) ir
            "function's return continuation is not a continuation, but instead ~a"
            (rcont ir))
      (verify (rcont ir))))
  (map-continuations #'verify ir))

(defmethod verify progn ((ir module))
  (map-functions #'verify ir))

(defun test-ninputs (ir expected)
  (test (eql (length (%uinputs ir)) expected) ir
        "has ~d inputs, not ~d as it should"
        (length (%uinputs expected)) expected))

(defmethod verify progn ((ir lookup)) (test-ninputs ir 2))
(defmethod verify progn ((ir cons)) (test-ninputs ir 2))
(defmethod verify progn ((ir car)) (test-ninputs ir 1))
(defmethod verify progn ((ir cdr)) (test-ninputs ir 1))
(defmethod verify progn ((ir enclose)) (test-ninputs ir 2))
(defmethod verify progn ((ir augment)) (test-ninputs ir 3))
(defmethod verify progn ((ir combination)) (test-ninputs ir 4))
(defmethod verify progn ((ir local-combination)) (test-ninputs ir 5))
(defmethod verify progn ((ir eval)) (test-ninputs ir 3))
(defmethod verify progn ((ir sequence)) (test-ninputs ir 3))
(defmethod verify progn ((ir continue)) (test-ninputs ir 2))
