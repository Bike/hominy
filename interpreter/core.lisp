(in-package #:burke/interpreter)

;;; Define a "core" environment. This contains definitions for parts of the standard library
;;; that are pretty intrinsic to the semantics of the language, versus modules, which while
;;; critical (e.g. numbers) are in a sense optional.
;;; Pretty vague criterion, admittedly. But basically it means stuff relating to the types
;;; used in the core evaluation algorithm: combiners, environments, symbols, lists.
;;; By which definition boolean stuff like $cond doesn't belong, actually...

(defvar *empty* (make-fixed-environment #() #()))

(defun k-apply (app list) (combine (unwrap app) list *empty*))

(defun k-eapply (app list env) (combine (unwrap app) list env))

(defun k-map (dynenv app &rest lists)
  (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
  (loop with comb = (unwrap app)
        for sublists = lists then (mapcar #'cdr lists)
        for items = (mapcar #'car sublists)
        collect (combine comb items dynenv)
        ;; FIXME: Kernel says error if the lists don't have the same length,
        ;; which is probably better.
        until (some #'null sublists)))

(defun k-not? (bool)
  (cond ((eq bool true) false)
        ((eq bool false) true)
        (t (error 'type-error :datum bool :expected-type 'boolean))))

(defun k-and? (&rest bools)
  (boolify
   (every (lambda (b)
            (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
            (eq b true))
          bools)))

(defun k-or? (&rest bools)
  (boolify
   (some (lambda (b)
           (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
           (eq b true))
         bools)))

(defun k-filter (app list)
  (let ((under (unwrap app)))
    (remove-if-not (lambda (elem) (combine under (list elem) *empty*)) list)))

(defun k-assoc (object list) (assoc object list :test #'equal))
(defun k-member? (object list) (boolify (member object list :test #'equal)))

(defun k-reduce (dynenv list binop id)
  (if (null list)
      id
      (let ((under (unwrap binop)))
        (reduce (lambda (o1 o2) (combine under (list o1 o2) dynenv)) list))))

(defun k-assq (object list) (assoc object list))
(defun k-memq? (object list) (boolify (member object list)))

(defun k-$binds? (dynenv envf sym)
  (boolify (binds? sym (eval envf dynenv))))

(defun k-for-each (dynenv app &rest lists)
  (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
  (loop with comb = (unwrap app)
        for sublists = lists then (mapcar #'cdr lists)
        for items = (mapcar #'car sublists)
        do (combine comb items dynenv)
        ;; FIXME: Kernel says error if the lists don't have the same length,
        ;; which is probably better.
        until (some #'null sublists)))

(defun make-standard-environment ()
  (let ((names nil) (values nil)
        (spack (find-package "BURKE/INTERPRETER/SYMS"))
        (ground (make-ground-environment)))
    (labels ((def (name value) (push name names) (push value values))
             (sym (sdesig) (intern (string sdesig) spack))
             (ground (sdesig) (lookup (sym sdesig) ground))
             (simp (f) (lambda (dynenv combinand) (apply f dynenv combinand)))
             (ign (f) (lambda (dynenv combinand)
                        (declare (cl:ignore dynenv))
                        (apply f combinand)))
             (op (f name) (make-builtin-operative f name))
             (mac (f name) (make-macro (op f name)))
             (app (f name) (wrap (op f name))))
      (macrolet ((defop (sdesig f)
                   (let ((name (intern (string sdesig) "BURKE/INTERPRETER/SYMS")))
                     `(def ',name (op ,f ',name))))
                 (defapp (sdesig f)
                   (let ((name (intern (string sdesig) "BURKE/INTERPRETER/SYMS")))
                     `(def ',name (app ,f ',name))))
                 (defmac (sdesig f)
                   (let ((name (intern (string sdesig) "BURKE/INTERPRETER/SYMS")))
                     `(def ',name (mac ,f ',name)))))
        (defapp #:list (ign #'list))
        (defapp #:list* (ign #'list*))
        (defmac #:$lambda (let ((wrap (ground ':wrap))
                                ($vau (ground ':$vau)))
                            (lambda (de combinand)
                              (declare (cl:ignore de))
                              (destructuring-bind (ptree . body) combinand
                                (list wrap (list* $vau ptree ignore body))))))
        (defapp #:caar (ign #'caar))
        (defapp #:cadr (ign #'cadr))
        (defapp #:cdar (ign #'cdar))
        (defapp #:cddr (ign #'cddr))
        (defapp #:caaar (ign #'caaar))
        (defapp #:caadr (ign #'caadr))
        (defapp #:cadar (ign #'cadar))
        (defapp #:caddr (ign #'caddr))
        (defapp #:cdaar (ign #'cdaar))
        (defapp #:cdadr (ign #'cdadr))
        (defapp #:cddar (ign #'cddar))
        (defapp #:cdddr (ign #'cdddr))
        (defapp #:caaaar (ign #'caaaar))
        (defapp #:caaadr (ign #'caaadr))
        (defapp #:caadar (ign #'caadar))
        (defapp #:caaddr (ign #'caaddr))
        (defapp #:cadaar (ign #'cadaar))
        (defapp #:cadadr (ign #'cadadr))
        (defapp #:caddar (ign #'caddar))
        (defapp #:cadddr (ign #'cadddr))
        (defapp #:cdaaar (ign #'cdaaar))
        (defapp #:cdaadr (ign #'cdaadr))
        (defapp #:cdadar (ign #'cdadar))
        (defapp #:cdaddr (ign #'cdaddr))
        (defapp #:cddaar (ign #'cddaar))
        (defapp #:cddadr (ign #'cddadr))
        (defapp #:cdddar (ign #'cdddar))
        (defapp #:cddddr (ign #'cddddr))
        (defapp #:apply (ign #'k-apply))
        (defapp #:eapply (ign #'k-eapply))
        (defmac #:$cond (let (($if (ground '#:$if))
                              ($sequence (ground '#:$sequence)))
                          (lambda (dynenv combinand)
                            (declare (cl:ignore dynenv))
                            (labels ((aux (comb)
                                       (if (null comb)
                                           inert
                                           (destructuring-bind ((test . body) . clauses)
                                               comb
                                             (list $if test (list* $sequence body)
                                                   (aux clauses))))))
                              (aux combinand)))))
        (defapp #:map (simp #'k-map))
        (defapp #:not? (ign #'k-not?))
        (defapp #:and? (ign #'k-and?))
        (defapp #:or? (ign #'k-or?))
        ;; Need gensyms or something to define $and? and $or? as macros - oop
        (defapp #:combiner? (ign (lambda (thing) (typep thing '(or operative applicative)))))
        (defapp #:append (ign #'append))
        (defapp #:filter (ign #'k-filter))
        (defapp #:assoc (ign #'k-assoc))
        (defapp #:member? (ign #'k-member?))
        (defapp #:reduce (simp #'k-reduce))
        (defapp #:append! (ign #'nconc))
        (defapp #:assq (ign #'k-assq))
        (defapp #:memq? (ign #'k-memq?))
        (defop  #:$binds? (simp #'k-$binds?))
        (defapp #:get-current-environment (simp (lambda (e) e)))
        (defmac #:$let* (let (($let (ground '#:$let)))
                          (lambda (dynenv combinand)
                            (declare (cl:ignore dynenv))
                            (destructuring-bind (bindings . body) combinand
                              (labels ((aux (bindings)
                                         (if (null bindings)
                                             (list* $let () body)
                                             (list $let (list (first bindings))
                                                   (aux (rest bindings))))))
                                (aux bindings))))))
        (defmac #:$letrec* (let (($letrec (ground '#:$letrec)))
                             (lambda (dynenv combinand)
                               (declare (cl:ignore dynenv))
                               (destructuring-bind (bindings . body) combinand
                                 (labels ((aux (bindings)
                                            (if (null bindings)
                                                (list* $letrec () body)
                                                (list $letrec (list (first bindings))
                                                      (aux (rest bindings))))))
                                   (aux bindings))))))
        (defapp #:for-each (simp #'k-for-each))))
    (make-fixed-environment names values ground (static-keying-environment))))
