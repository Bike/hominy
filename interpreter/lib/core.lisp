(in-package #:burke/interpreter)

;;; Define a "core" environment. This contains definitions for parts of the standard library
;;; that are pretty intrinsic to the semantics of the language, versus modules, which while
;;; critical (e.g. numbers) are in a sense optional.
;;; Pretty vague criterion, admittedly. But basically it means stuff relating to the types
;;; used in the core evaluation algorithm: combiners, environments, symbols, lists.
;;; By which definition boolean stuff like $cond doesn't belong, actually...

(defvar *empty* (make-fixed-environment #() #()))

(defenv *core* (*ground*)
  (defapp list (&rest elems) ignore ignore elems)
  (defapp list* (&rest elems) ignore ignore (apply #'list* elems))
  (let ((wrap (lookup 'syms::wrap *ground*))
        ($vau (lookup 'syms::$vau *ground*)))
    (defmac $lambda (ptree &rest body) ignore ignore
      (list wrap (list* $vau ptree ignore body))))
  (macrolet ((defc (name) `(defapp ,name (list) ignore ignore (,name list)))
             (defcs (&rest names)
               `(progn ,@(loop for name in names collect `(defc ,name)))))
    (defcs caar cadr cdar cddr
      caaar caadr cadar caddr cdaar cdadr cddar cdddr
      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
  (defapp apply (applicative list) ignore frame
    (combine (unwrap applicative) list *empty* frame))
  (defapp eapply (applicative list) env frame
    (combine (unwrap applicative) list env frame))
  (let (($if (lookup 'syms::$if *ground*))
        ($sequence (lookup 'syms::$sequence *ground*)))
    (defmac $cond (&rest clauses) ignore ignore
      (labels ((aux (comb)
                 (if (null comb)
                     inert
                     (destructuring-bind ((test . body) . clauses)
                         comb
                       (list $if test (list* $sequence body)
                             (aux clauses))))))
        (aux clauses))))
  (defapp map (app &rest lists) dynenv ignore
    ;; FIXME: Frames for the map combinations
    (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
    (loop with comb = (unwrap app)
          for sublists = lists then (mapcar #'cdr sublists)
          for items = (mapcar #'car sublists)
          ;; FIXME: Kernel says error if the lists don't have the same length,
          ;; which is probably better.
          until (some #'null sublists)
          collect (combine comb items dynenv)))
  (defapp not? (bool) ignore ignore
    (cond ((eq bool true) false)
          ((eq bool false) true)
          (t (error 'type-error :datum bool :expected-type 'boolean))))
  (defapp and? (&rest bools) ignore ignore
    (boolify
     (every (lambda (b)
              (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
              (eq b true))
            bools)))
  (defapp or? (&rest bools) ignore ignore
    (boolify
     (some (lambda (b)
             (unless (typep b 'boolean) (error 'type-error :datum b :expected-type 'boolean))
             (eq b true))
           bools)))
  ;; KLUDGE for recursive definitions.
  (let* (($and? (make-instance 'macro))
         ($if (lookup 'syms::$if *ground*))
         (body
           (lambda (dynenv frame bools)
             (declare (cl:ignore dynenv frame))
             (cond ((null bools) true)
                   ((null (cdr bools)) (first bools)) ; tail context
                   (t (list $if (first bools) (list* $and? (rest bools)) false)))))
         (op (make-builtin-operative body 'syms::$and?)))
    (setf (%expander $and?) op)
    (define $and? 'syms::$and? *defining-environment*))
  (let* (($or? (make-instance 'macro))
         ($if (lookup 'syms::$if *ground*))
         (body
           (lambda (dynenv frame bools)
             (declare (cl:ignore dynenv frame))
             (cond ((null bools) false)
                   ((null (cdr bools)) (first bools)) ; tail context
                   (t (list $if (first bools) true (list* $or? (rest bools)))))))
         (op (make-builtin-operative body 'syms::$or?)))
    (setf (%expander $or?) op)
    (define $or? 'syms::$or? *defining-environment*))
  (defapp combiner? (object) ignore ignore (boolify (typep object 'combiner)))
  (defapp append (&rest lists) ignore ignore (reduce #'append lists))
  (defapp filter (app list) ignore ignore
    (let ((under (unwrap app)))
      (remove-if-not (lambda (elem) (combine under (list elem) *empty*)) list)))
  (defapp reduce (list binop id) dynenv frame
    ;; FIXME: Frames for combinations
    (if (null list)
        id
        (let ((under (unwrap binop)))
          (reduce (lambda (o1 o2) (combine under (list o1 o2) dynenv frame))
                  list))))
  (defapp append! (&rest lists) ignore ignore (reduce #'nconc lists))
  (defapp assq (object list) ignore ignore (assoc object list))
  (defapp memq? (object list) ignore ignore (boolify (member object list)))
  (defop  $binds? (env sym) dynenv ignore (boolify (binds? sym (eval env dynenv))))
  (defapp get-current-environment () env ignore env)
  (let (($let (lookup 'syms::$let *ground*)))
    (defmac $let* (bindings &rest body) ignore ignore
      (labels ((aux (bindings)
                 (if (null bindings)
                     (list* $let () body)
                     (list $let (list (first bindings))
                           (aux (rest bindings))))))
        (aux bindings))))
  ;; This has slightly different behavior from Kernel with respect to forms
  ;; that immediately evaluate the newly bound names. In Kernel, doing such will
  ;; get you the outside binding value if there is one, or else error with an
  ;; unbound variable. (This is not stated outright but is the behavior of the
  ;; given derivation.) This here binds everything to #inert. I think the ideal
  ;; would be to signal an error. To do that, either there needs to be a special
  ;; "unbound" marker to put in temporarily, or something like symbol macros.
  (let (($let (lookup 'syms::$let *ground*))
        ($set! (lookup 'syms::$set! *ground*))
        (list (lookup 'syms::list *defining-environment*)))
    (defmac $letrec (bindings &rest body) ignore ignore
      (list* $let (mapcar (lambda (bind) (list (first bind) inert)) bindings)
             (list $set! (mapcar #'first bindings)
                   (list* list (mapcar #'second bindings)))
             body)))
  (let (($letrec (lookup 'syms::$letrec *defining-environment*)))
    (defmac $letrec* (bindings &rest body) ignore ignore
      (labels ((aux (bindings)
                 (if (null bindings)
                     (list* $letrec () body)
                     (list $letrec (list (first bindings))
                           (aux (rest bindings))))))
        (aux bindings))))
  (defapp for-each (app &rest lists) dynenv frame
    ;; FIXME: Frames
    (when (null lists) (error 'type-error :datum lists :expected-type 'cons))
    (loop with comb = (unwrap app)
          for sublists = lists then (mapcar #'cdr lists)
          for items = (mapcar #'car sublists)
          do (combine comb items dynenv frame)
             ;; FIXME: Kernel says error if the lists don't have the same length,
             ;; which is probably better.
          until (some #'null sublists))
    inert)
  ;; Establish a lexically bound escape, like cl:block.
  ;; You still use just THROW to get to it, though.
  ;; FIXME: Might be better to give these an encapsulated type,
  ;; but on the other hand that would make using them with dconts awkward.
  ;; FIXME: In the final version ought to use static keys, not a gensym.
  (let (($catch (lookup 'syms::$catch *ground*))
        ($make-catch-tag (lookup 'syms::$make-catch-tag *ground*))
        ($let (lookup 'syms::$let *ground*)))
    (defmac $let/ec (block-name &rest body) ignore ignore
      (let ((csym (gensym "CATCH")))
        #+(or)
        `($let (((,csym ,block-name) ($make-catch-tag ,block-name)))
           ($catch ,csym ,@body))
        (list $let (list (list (list csym block-name)
                               (list $make-catch-tag block-name)))
              (list* $catch csym body))))))
