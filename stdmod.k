($export! (eval apply call eapply ecall $cond map $let not? and? or?
                $and? $or? combiner? append filter assoc acons member? reduce copy-es
                assq memq? get-current-environment $let* $letrec*
                $let-redirect $let-safe $remote-eval $bindings->environment))

($define! eval ($lambda (form env)
                 ($cond ((symbol? form) (lookup form env))
                        ((pair? form) (combine (car form) (cdr form) env))
                        (#t form))))

;;; apply and eapply are split, unlike in shutt where the dynenv is an
;;; optional parameter. i think optional parameters are kind of ugly when
;;; there's no syntax for them, especially for such a fundamental operator.
;;; in particular, i don't think shutt's signals an error on too many args.

($define! empty-env (make-fixed-environment () ()))

($define! apply ($lambda (app args) (combine (unwrap app) args empty-env)))
($define! eapply ($lambda (app args dynenv) (combine (unwrap app) args dynenv)))

($define! call ($lambda (app . args) (combine (unwrap app) args empty-env)))
($define! ecall ($lambda (app dynenv . args) (combine (unwrap app) args empty-env)))

($define! $cond
  ($vau clauses env
    ($if (null? clauses)
         #inert
         ($let ((((test . body) . clauses) clauses))
           ($if (eval test env)
                (combine $sequence body env)
                (combine $cond clauses env))))))

($define! map1
  (wrap ($vau (app list) dynenv
          ($if (null? list)
               ()
               (cons (ecall app dynenv (car list))
                     (ecall map1 dynenv app (cdr list)))))))

($define! map
  (wrap ($vau (app . lists) dynenv
          ($if (apply or? (map1 null? lists))
               ()
               (cons (eapply app (map1 car lists) dynenv)
                     (eapply map (list* app (map1 cdr lists)) dynenv))))))

($define! $let
  ($macro (bindings . body) #ignore
    (list* (list* $lambda (map car bindings) body) (map cadr bindings))))

($define! not? ($lambda (x) ($if x #f #t)))

($define! and?
  ($lambda bools
    ($cond ((null? bools) #t)
           ((car bools) (apply and? (cdr bools)))
           (#t #f))))

($define! $and?
  ($macro bools #ignore
    ($cond ((null? bools) #t)
           ((null? (cdr bools)) (car bools)) ; tail context
           (#t (list $if (car bools) (list* $and? (cdr bools)) #t)))))

($define! $or?
  ($macro bools #ignore
    ($cond ((null? bools) #f)
           ((null? (cdr bools)) (car bools)) ; tail
           (#t (list $if (car bools) #t (list* $or? (cdr bools)))))))

($define! combiner?/1 ($lambda (x) (or? (applicative? x) (operative? x))))

($define! combiner? ($lambda args (apply and? (map combiner?/1 args))))

($define! append2
  ($lambda (list1 list2)
    ($if (null? list1)
         list2
         (cons (car list1) (append2 (cdr list1) list2)))))

($define! append ($lambda lists (reduce append2 lists ())))

;; Different from Kernel's, which constructs a new empty environment for
;; every call.
($define! filter
  (wrap ($vau (app list) dynenv
          ($cond ((null? list) list)
                 ((ecall app dynenv (car list))
                  (cons (car list)
                        (ecall filter dynenv app (cdr list))))
                 (#t (ecall filter dynenv app (cdr list)))))))

($define! assoc
  ($lambda (object pairs)
    ($if (null? pairs)
         ()
         ($let (((item . pairs) pairs))
           ($if (equal? (car item) object)
                item
                (assoc object pairs))))))

($define! acons ($lambda (key value alist) (cons (cons key value) alist)))

($define! member? ($lambda (object list)
                    ($cond ((null? list) #f)
                           ((equal? object (car list)) #t)
                           (#t (member? object (cdr list))))))

($define! reduce
  ($lambda (binary list identity)
    ($cond ((null? list) identity)
           ((null? (cdr list)) (car list))
           (#t (call binary (car list)
                     (reduce binary (cdr list) identity))))))

($define! copy-es
  ($lambda (obj)
    ($letrec ((aux
               ($lambda (x alist)
                 ($if (not? (cons? x))
                      (list x alist)
                      ($let ((record (assoc x alist)))
                        ($if (cons? record)
                             (list (cdr record) alist)
                             ($let* ((y (cons () ()))
                                     (alist (acons x y alist))
                                     ((z alist) (aux (car x) alist))
                                     (#ignore (set-car! y z))
                                     ((z alist) (aux (cdr x) alist))
                                     (#ignore (set-cdr! y z)))
                               (list y alist))))))))
      (car (aux obj ())))))

($define! assq
  ($lambda (obj alist)
    ($if (null? alist)
         ()
         ($let (((item . alist) alist))
           ($if (eq? obj (car item))
                item
                (assq obj (cdr alist)))))))

($define! memq?
  ($lambda (obj list)
    ($cond ((null? list) #f)
           ((eq? obj (car list)) #t)
           (#t (memq? obj (cdr list))))))

($define! get-current-environment (wrap ($vau () e e)))

($define! $let*
  ($macro (bindings . body) #ignore
    ($if (null? bindings)
         (list* $let bindings body)
         (list $let (list (car bindings))
               (list* $let* (cdr bindings) body)))))

($define! $letrec*
  ($macro (bindings . body) #ignore
    ($if (null? bindings)
         (list* letrec$ bindings body)
         (list $letrec
               (list (car bindings))
               (list* $letrec* (cdr bindings) body)))))

;; This definition particular IMO shows how combine can be clearer than eval,
;; contra shutt's comment (in apply or something)
($define! $let-redirect
  ($vau (env bindings . body) dynenv
    (combine (combine $lambda
                      (list* (map car bindings) body)
                      (eval env dynenv))
             (map cadr bindings)
             dynenv)))

($define! $let-safe
  ($vau (bindings . body) env
    (combine $let-redirect (list* (make-kernel-standard-environment)
                                  bindings body)
             env)))

($define! $remote-eval ($vau (form env) dynenv (eval form (eval env dynenv))))

;; Alternately we could expand into make-fixed-environment, but then we'd
;; need quasiquotation, or a more operative version of m-f-e.
($define! $bindings->environment
  ($macro bindings #ignore
    (list $let-redirect
          (list make-environment) bindings (list get-current-environment))))
