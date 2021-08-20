(in-package #:burke)

#|
($vau (x . y) e form)
becomes, with % meaning internal/artificial, and caps meaning a mutable var
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %e0
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $eval := lookup '$eval %closure-environment
 %result := call $eval '#<empty env> 'form %e0 ; "environment call"
 ret %result

where $eval is (unwrap eval) and $augment is (unwrap augment). Now say form is (list x y e). Since the type of the argument to $eval is a cons, a rewriter on $eval comes in:
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $car := lookup '$car %closure-environment
 $cdr := lookup '$cdr %closure-environment
 $eval := lookup '$eval %closure-environment
 $combine := lookup '$combine %closure-environment
 %combinerf := call $car '#<empty env> '(list x y e)
 %combiner := call $eval '#<empty env> %combinerf %e0
 %combinand := call $cdr '#<empty env> '(list x y e)
 %result := call $combine '#<empty env> %combiner %combinand %e2
 ret %result

Constant folding is applied on the car and cdr:
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $eval := lookup '$eval %closure-environment
 $combine := lookup '$combine %closure-environment
 %combiner := call $eval '#<empty env> 'list %e0
 %result := call $combine '#<empty env> %combiner '(x y e) %e0
 ret %result

Since the eval's argument has type symbol, it can be rewritten:
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $combine := lookup '$combine %closure-environment
 %combiner := lookup 'list %e0
 %result := call $combine '#<empty env> %combiner '(x y e) %e0
 ret %result

%E0's type indicates that it does not have "LIST" bound, so this can be rewritten to read from the closure environment
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $combine := lookup '$combine %closure-environment
 %combiner := lookup 'list %closure-environment
 %result := call $combine '#<empty env> %combiner '(x y e) %e0
 ret %result

A rewriter on the $COMBINE type sees the combinand is a list of constant length and rewrites as a call
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 %combiner := lookup 'list %closure-environment
 $car := lookup '$car %closure-environment
 $cdr := lookup '$cdr %closure-environment
 %arg0 := call $car '#<empty env> '(x y e)
 %args1 := call $cdr '#<empty env> '(x y e)
 %arg1 := call $car '#<empty env> %args1
 %args2 := call $cdr '#<empty env> %args1
 %arg2 := call $car '#<empty env> %args2
 %result := call %combiner %e0 %arg0 %arg1 %arg2
 ret %result

Constant folds are applied
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 %combiner := lookup 'list %closure-environment
 %result := call %combiner %e0 'x 'y 'e
 ret %result

In the closure environment's type we can see that LIST is a wrap of an operative
$LIST, so we can rewrite the call
cfunction(x . y, e, %closure-environment)
 $augment = lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 %combiner := lookup '$list %closure-environment
 $eval := lookup '$eval %closure-environment
 %arg0 := call $eval '#<empty env> 'x %e0
 %arg1 := call $eval '#<empty env> 'y %e0
 %arg2 := call $eval '#<empty env> 'e %e0
 %result := call %combiner %e0 %arg0 %arg1 %arg2
 ret %result

The evals can be rewritten as lookups similarly to above stuff
cfunction(x . y, e, %closure-environment)
 $augment := lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $list := lookup '$list %closure-environment ; renamed, why not
 %arg0 := lookup 'x %e0
 %arg1 := lookup 'y %e0
 %arg2 := lookup 'e %e0
 %result := call $list %e0 %arg0 %arg1 %arg2
 ret %result

The lookups can be resolved because the environment is local and created by an augment call with constant variable names (hardest step so far?)
cfunction(x . y, e, %closure-environment)
 $augment := lookup '$augment %closure-environment
 %e0 := augment %closure-environment 'x x 'y y 'e e
 $list := lookup '$list %closure-environment
 %result := call $list %e0 x y e ; wow!!
 ret %result

Since $LIST's type indicates that it ignores the environment, we can use a null one
cfunction(x . y, e, %closure-environment)
 $augment := lookup '$augment %closure-environment
 %e0 := call $augment '#<empty env> %closure-environment 'x x 'y y 'e e
 $list := lookup '$list %closure-environment
 %result := call $list '#<empty env> x y e
 ret %result

the augment call is no longer used, so it can be deleted
cfunction(x . y, e, %closure-environment)
 $list := lookup '$list %closure-environment
 %result := call $list '#<empty env> x y e
 ret %result

which finally looks like something a normal-ass efficient programming language
would start with.
Now lower stages can do things like figure out an efficient lookup and calling convention.
|#

;;; This is the initial function all user functions are derived from. It just
;;; calls eval on its body, basically.
;;; The enclosed data is (static-env plist eparam . body).

(defun fresh-function (&optional fname)
  (burke/ir:assemble ((f fname) enclosed return)
    (start (combinand.dynenv)
     ()
     (:= static-env (ir:car enclosed))
     (:= t0 (ir:cdr enclosed))
     (:= plist (ir:car t0))
     (:= t1 (ir:cdr t0))
     (:= eparam (ir:car t1))
     (:= body (ir:cdr t1))
     (:= aplist (ir:cons plist eparam))
     (:= env (ir:augment static-env aplist combinand.dynenv))
     (ir:sequence return body env))))
