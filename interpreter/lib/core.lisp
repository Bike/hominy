(in-package #:burke/interpreter)

;;; Define a "core" environment. This contains definitions for parts of the standard library
;;; that are pretty intrinsic to the semantics of the language, versus modules, which while
;;; critical (e.g. numbers) are in a sense optional.
;;; Pretty vague criterion, admittedly. But basically it means stuff relating to the types
;;; used in the core evaluation algorithm: combiners, environments, symbols, lists.
;;; By which definition boolean stuff like $cond doesn't belong, actually...

(defvar *empty* (make-fixed-environment #() #()))

(defenv *core* (*ground*)
  (defapp list (&rest elems) ignore elems)
  (defapp list* (&rest elems) ignore (apply #'list* elems))
  (let ((wrap (lookup 'syms::wrap *ground*))
        ($vau (lookup 'syms::$vau *ground*)))
    (defmac $lambda (ptree &rest body) ignore
      (list wrap (list* $vau ptree ignore body))))
  (macrolet ((defc (name) `(defapp ,name (list) ignore (,name list)))
             (defcs (&rest names)
               `(progn ,@(loop for name in names collect `(defc ,name)))))
    (defcs caar cadr cdar cddr
      caaar caadr cadar caddr cdaar cdadr cddar cdddr
      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
  (defapp apply (applicative list) ignore (combine (unwrap applicative) list *empty*))
  (defapp eapply (applicative list) env (combine (unwrap applicative) list env))
  (let (($if (lookup 'syms::$if *ground*))
        ($sequence (lookup 'syms::$sequence *ground*)))
    (defmac $cond (&rest clauses) ignore
      (labels ((aux (comb)
                 (if (null comb)
                     inert
                     (destructuring-bind ((test . body) . clauses)
                         comb
                       (list $if test (list* $sequence body)
                             (aux clauses))))))
        (aux clauses))))
  (defapp map (app &rest lists) dynenv
    (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
    (loop with comb = (unwrap app)
          for sublists = lists then (mapcar #'cdr lists)
          for items = (mapcar #'car sublists)
          collect (combine comb items dynenv)
          ;; FIXME: Kernel says error if the lists don't have the same length,
          ;; which is probably better.
          until (some #'null sublists)))
  (defapp not? (bool) ignore
    (cond ((eq bool true) false)
          ((eq bool false) true)
          (t (error 'type-error :datum bool :expected-type 'boolean))))
  (defapp and? (&rest bools) ignore
    (boolify
     (every (lambda (b)
              (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
              (eq b true))
            bools)))
  (defapp or? (&rest bools) ignore
    (boolify
     (some (lambda (b)
             (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
             (eq b true))
           bools)))
  ;; KLUDGE for recursive definitions.
  (let* (($and? (make-instance 'macro))
         ($if (lookup 'syms::$if *ground*))
         (body
           (lambda (dynenv bools)
             (declare (cl:ignore dynenv))
             (cond ((null bools) true)
                   ((null (cdr bools)) (first bools)) ; tail context
                   (t (list $if (first bools) (list* $and? (rest bools)) false)))))
         (op (make-builtin-operative body 'syms::$and?)))
    (setf (%expander $and?) op)
    (define $and? 'syms::$and? *defining-environment*))
  (let* (($or? (make-instance 'macro))
         ($if (lookup 'syms::$if *ground*))
         (body
           (lambda (dynenv bools)
             (declare (cl:ignore dynenv))
             (cond ((null bools) false)
                   ((null (cdr bools)) (first bools)) ; tail context
                   (t (list $if (first bools) true (list* $or? (rest bools)))))))
         (op (make-builtin-operative body 'syms::$or?)))
    (setf (%expander $or?) op)
    (define $or? 'syms::$or? *defining-environment*))
  (defapp combiner? (object) ignore (boolify (typep object 'combiner)))
  (defapp append (&rest lists) ignore (reduce #'append lists))
  (defapp filter (app list) ignore
    (let ((under (unwrap app)))
      (remove-if-not (lambda (elem) (combine under (list elem) *empty*)) list)))
  (defapp reduce (list binop id) dynenv
    (if (null list)
        id
        (let ((under (unwrap binop)))
          (reduce (lambda (o1 o2) (combine under (list o1 o2) dynenv)) list))))
  (defapp append! (&rest lists) ignore (reduce #'nconc lists))
  (defapp assq (object list) ignore (assoc object list))
  (defapp memq? (object list) ignore (boolify (member object list)))
  (defop  $binds? (env sym) dynenv (boolify (binds? sym (eval env dynenv))))
  (defapp get-current-environment () env env)
  (let (($let (lookup 'syms::$let *ground*)))
    (defmac $let* (bindings &rest body) ignore
      (labels ((aux (bindings)
                 (if (null bindings)
                     (list* $let () body)
                     (list $let (list (first bindings))
                           (aux (rest bindings))))))
        (aux bindings))))
  (let (($letrec (lookup 'syms::$letrec *ground*)))
    (defmac $letrec* (bindings &rest body) ignore
      (labels ((aux (bindings)
                 (if (null bindings)
                     (list* $letrec () body)
                     (list $letrec (list (first bindings))
                           (aux (rest bindings))))))
        (aux bindings))))
  (defapp for-each (app &rest lists) dynenv
    (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
    (loop with comb = (unwrap app)
          for sublists = lists then (mapcar #'cdr lists)
          for items = (mapcar #'car sublists)
          do (combine comb items dynenv)
             ;; FIXME: Kernel says error if the lists don't have the same length,
             ;; which is probably better.
          until (some #'null sublists))
    inert))
