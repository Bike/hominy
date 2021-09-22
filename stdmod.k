($module (eval apply $cond ...)
  (eval ($lambda (form env)
          ($cond ((symbol? form) (lookup form env))
                 ((cons? form) (combine (car form) (cdr form) env))
                 (#t form))))
  ;; This is different from Shutt's definition: it captures the dynamic
  ;; environment. I think this is actually more regular, but we'll see.
  ;; Assuming an actual inliner, this capture shouldn't be any more of a problem
  ;; efficiency-wise than that possible with any normal combination.
  ;; It allows a very literal specification of the dynenv-preserving property
  ;; of e.g. cl:map.
  ;; Safety-wise, with an argument applicative we should always specify a
  ;; dynamic environment rather than use one from inside a library definition.
  ;; Actually this is the case for any code, i.e. apply and call should only be
  ;; used if you're really sure about what you're calling. This implies that
  ;; any higher order function needs to be defined with wrap $vau, but that is
  ;; pretty inconvenient.
  (apply (wrap ($vau (app args) dynenv) (combine (unwrap app) args dynenv)))
  (call (wrap ($vau (app . args) dynenv) (combine (unwrap app) args dynenv)))
  (eapply ($lambda (app args dynenv) (combine (unwrap app) args dynenv)))
  (ecall ($lambda (app dynenv . args) (combine (unwrap app) args dynenv)))
  ($cond ($vau clauses env
           ($if (null? clauses)
                #inert
                ($let ((((test . body) . clauses) clauses))
                  ($if (eval test env)
                       (combine $sequence body env)
                       (combine $cond clauses env))))))
  (map1 (wrap ($vau (app list) dynenv
                ($if (null? list)
                     ()
                     (cons (ecall app dynenv (car list))
                           (ecall map1 dynenv app (cdr list)))))))
  (map (wrap ($vau (app . lists) dynenv
               ($if (apply or? (map1 null? lists))
                    ()
                    (cons (eapply app (map1 car lists) dynenv)
                          (eapply map (list* app (map1 cdr lists)) dynenv))))))
  ($let ($vau (bindings . body) env
          (combine (combine $lambda (list* (map car bindings) body) env)
                   (map cadr bindings)
                   env)))
  (not? ($lambda (x) ($if x #f #t)))
  (and? ($lambda bools
          ($cond ((null? bools) #t)
                 ((car bools) (apply and? (cdr bools)))
                 (#t #f))))
  (or? ($lambda bools
         ($cond ((null? bools) #f)
                ((car bools) #t)
                (#t (apply or? (cdr bools))))))
  ($and? ($vau bools env
           ($cond ((null? bools) #t)
                  ((null? (cdr bools)) (eval (car bools) env)) ; tail
                  ((eval (car bools) env) (combine $and? (cdr bools) env))
                  (#t #f))))
  ($or? ($vau bools env
          ($cond ((null? bools) #f)
                 ((null? (cdr bools)) (eval (car bools) env))
                 ((eval (car bools) env) #t)
                 (#t (combine $or? (cdr bools) env)))))
  (combiner? ($lambda args
               (apply and? (map ($lambda (x)
                                  (or? (applicative? x) (operative? x)))
                                args))))
  (append2 ($lambda (list1 list2)
             ($if (null? list1)
                  list2
                  (cons (car list1) (append2 (cdr list1) list2)))))
  (append ($lambda lists (reduce append2 lists ())))
  ;; Different from Kernel's, which constructs a new empty environment for
  ;; every call.
  (filter (wrap ($vau (app list) dynenv
                  ($cond ((null? list) list)
                         ((ecall app dynenv (car list))
                          (cons (car list)
                                (ecall filter dynenv app (cdr list))))
                         (#t (ecall filter dynenv app (cdr list)))))))
  (assoc ($lambda (object pairs)
           ($if (null? pairs)
                ()
                ($let (((item . pairs) pairs))
                  ($if (equal? (car item) object)
                       item
                       (assoc object pairs))))))
  (acons ($lambda (key value alist) (cons (cons key value) alist)))
  (member? ($lambda (object list)
             ($cond ((null? list) #f)
                    ((equal? object (car list)) #t)
                    (#t (member? object (cdr list))))))
  (reduce ($lambda (binary list identity)
            ($cond ((null? list) identity)
                   ((null? (cdr list)) (car list))
                   (#t (call binary (car list)
                             (reduce binary (cdr list) identity))))))
  (copy-es ($lambda (obj)
             ($letrec ((aux (x alist)
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
                                       (list y alist)))))))
               (car (aux obj ())))))
  (assq ($lambda (obj alist)
          ($if (null? alist)
               ()
               ($let (((item . alist) alist))
                 ($if (eq? obj (car item))
                      item
                      (assq obj (cdr alist)))))))
  (memq? ($lambda (obj list)
           ($cond ((null? list) #f)
                  ((eq? obj (car list)) #t)
                  (#t (memq? obj (cdr list))))))
  (get-current-environment (wrap ($vau () e e)))
  (make-kernel-standard-environment ($lambda () (get-current-environment)))
  ($let* ($vau (bindings . body) env
           (eval ($if (null? bindings)
                      (list* let$ bindings body)
                      (list $let
                            (list (car bindings))
                            (list* $let* (cdr bindings) body)))
                 env)))
  ;; Alternate, less macro-y definition
  ($let* ($vau (bindings . body) env
           ;; We have to make a new environment even if $bindings is (), so we
           ;; can't just use $sequence.
           (combine $let
                    ($if (null? bindings)
                         (list* bindings body)
                         (list (list (car bindings))
                               (combine $let* (list* (cdr bindings) body) env)))
                    env)))
  ($letrec* ($vau (bindings . body) env
              (eval ($if (null? bindings)
                         (list* letrec$ bindings body)
                         (list $letrec
                               (list (car bindings))
                               (list* $letrec* (cdr bindings) body)))
                    env)))
  ;; This definition particular IMO shows how combine can be clearer than eval,
  ;; contra shutt's comment (in apply or something)
  ($let-redirect
    ($vau (env bindings . body) dynenv
      (combine (combine $lambda
                        (list* (map car bindings) body)
                        (eval env dynenv))
               (map cadr bindings)
               dynenv)))
  ($let-safe ($vau (bindings . body) env
               (combine $let-redirect (list* (make-kernel-standard-environment)
                                             bindings body)
                        env)))
  ($remote-eval ($vau (form env) dynenv (eval form (eval env dynenv))))
  ($bindings->environment
    ($vau bindings denv
      (combine $let-redirect (list (make-environment)
                                   bindings
                                   (list get-current-environment))
               denv))))

($module (promise? memoize $lazy force)
  ;; multiple definitions at once
  ;; ngl i do not understand this and shutt's shitty variable names don't help
  ;; OK let's work through this. A promise is (list (cons obj env)). If env is
  ;; () it means the promise has been forced already and obj is the value.
  ;; Otherwise, upon forcing we evaluate the obj in the env. Doing so may
  ;; recursively force the promise, in which case we use that earlier result.
  ;; If not, the result might be a promise, in which case we recursively force
  ;; that; that recursive forcing is done by discarding the current promise's
  ;; CAR in favor of the recursive one's, so that forcing it properly forces
  ;; both (rather than only forcing the current promise). And finally if the
  ;; result is just a value, that's stored in the cons along with () to
  ;; indicate the forcing is complete.
  ;; Shutt also mentions that handle-promise-result has to tail call
  ;; force-promise in order for indefinite iteration of promises to work, which
  ;; was apparently another problem with the R5RS example implementation.
  (((encapsulate promise? decapsulate) (make-encapsulation-type))
   (memoize ($lambda (value) (encapsulate (list (list value)))))
   ($lazy ($vau (exp) env (encapsulate (list (cons exp env)))))
   (force ($lambda (object)
     ($if (promise? object)
          (force-promise (decapsulate object))
          object)))
   (force-promise
     ($lambda (%promise)
       ($let ((((object . env)) %promise))
         ($if (environment? env)
              (handle-promise-result %promise (eval object env))
              object))))
   (handle-promise-result
     ($lambda (%promise value)
       ($cond ((null? (cdar %promise)) ; check for earlier result
               (caar %promise))
              ((promise? value) ; iterate
               (set-car! %promise (car (decapsulate y)))
               (force-promise %promise))
              (#t ; memoize
               (set-car! (car %promise) value)
               (set-cdr! (car x) ())
               value))))))
