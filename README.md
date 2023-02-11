This is an implementation of a language resembling [Kernel](https://web.cs.wpi.edu/~jshutt/kernel.html). The implementation includes an interpreter and soon a compiler.

# How to use

Load the `hominy` system. Call `(hominy:repl)` to get a Hominy REPL with the base library, which includes some of the operatives etc. in the Kernel report and some other stuff also, which is not fully documented. As of this writing, the parts of the Kernel report implemented are most of the "core library features", as well as a version of keyed static variables. Use `(exit)` to exit the REPL.

`hominy:read`, `hominy:read-from-string`, `hominy/interpreter:eval`, `hominy/interpreter:lookup`, and `hominy/interpreter:combine` can be employed together to execute Hominy code from within the host Lisp REPL.

## The compiler

So far only the "tree" compiler is remotely operable. It can do some basic optimizations like avoiding consing environments in basic cases, reducing combinations of basic operatives like `$let` or `$if`, etc.

To use it, try `(hominy:repl :modules (list (hominy/cenv:module) (hominy/treec:module)))`. Then you will be able to use the `compile` applicative and the `standard-compilation-environment`. To compile a combiner, do `(compile combiner-goes-here standard-compilation-environment)` and you will get a compiled combiner back.

For now the only real target for Hominy is a VM.

# What is this?

Kernel is an experimental programming language developed in the 2000s by Dr. John Shutt. You can read his draft specification and doctoral thesis at the link above.

To summarize, it is a Lisp which reverses our usual thinking about quotation. Normal Lisps (Common Lisp, Scheme, whatever) are based on lambda calculus with the addition of a quotation operator that suppresses computation. In practice, evaluation is the norm - we usually define functions, and the arguments of functions are evaluated. We have to go out of our way to do otherwise - either with quotation, defining something as a macro, or a limited number of operators specially understood by the compiler. Kernel, however, is based on "vau" calculus (it's an obscure Greek letter - died out before Homer - how about that). In vau calculus, the basic abstraction operator (vau rather than lambda) does _not_ have its arguments evaluated, and you have to go out of your way to indicate that something should be evaluated - either by using `eval`, or more often, "wrapping" a vau abstraction to indicate that its arguments should be evaluated.

The upshot of this is that Kernel does not have what Lisps call "special operators" - operators that are specifically hardcoded into the compiler/evaluator. Every operator is special, and receives unevaluated arguments. Of course, practically speaking we want most things to behave like functions, and Kernel still has a lambda operator (though called `$lambda` in a fit of Hungarian notation) and so on, but the fundamental semantics are in terms of these vau things that recieve objects unevaluated.

Just to make this perfectly clear, here is an example of the differing behavior of identity functions written with `$vau` and `$lambda`. (Don't worry about the `#ignore`.)

```lisp
($let ((a 4)) (($lambda (x) x) a)) => 4
($let ((a 4)) (($vau (x) #ignore x) a)) => a
```

# Elaborate some more

Kernel accomplishes this with two innovations.

First, Kernel divides operators (that is, things that appear as the car of a form) into "operatives" and "applicatives". "applicatives" are what plebs like myself recognize as normal functions, which have their arguments evaluated. "operatives" are created by the `$vau` operator and receive unevaluated operands. An operative can be "wrapped" in an applicative using the `wrap` applicative. Every applicative - every normal function - is just a wrapped operative. `($lambda params . body)` is defined as `(wrap ($vau params #ignore . body))`. Operatives are first class, unlike special operators in any Lisp I've used. (Applicatives are also first class.)

Second, in order for normal control operators to exist, environments are reified into first class objects as well, and `eval` requires one to work. Operatives constructed by `$vau` may receive the dynamic environment (i.e. the environment the call to the operative was evaluated in) as a parameter and use it for evaluations or whatever else.

To illustrate, here is Kernel's definition of `$cond`, which is just the usual Lisp `cond` multi choice operator:

```lisp
($vau clauses env
  ($if (null? clauses)
       #inert
       ($let* ((clause (car clauses))
               (test (car clause))
               (body (cdr clause))
               (clauses (cdr clauses)))
         ($if (eval test env)
              (eval (cons $sequence body) env)
              (eval (cons $cond clauses) env)))))
```

Now say we have the form `($cond ((integer? x) 0) ((symbol? x) 1))`, which returns a sort of tag for an object. `$cond` receives the unevaluated structure `((integer? x) 0) ((symbol? x) 1))` as its "clauses" argument, and the dynamic environment (which presumably binds `x`) as "env". The clauses are not null, so we go to the second bit of the first `$if`. This extracts `(integer? x)` as `test`, `(0)` as `body`, and `((symbol? x) 1)` as the remaining clauses. It evaluates `(integer? x)` in the dynamic environment; in this case, this will get `x`'s value and pass it to the `integer?` predicate. If the value is in fact a integer, `($sequence 0)` is evaluated, and this will return `0`. Otherwise `($cond ((symbol? x) 1))` is evaluated, and the process continues until a clause succeeds or there are no more clauses.

For more detailed information, you should read Dr. Shutt's materials, which are more comprehensive than anything I could write. You can also check interpret.lisp in this project, which implements a simple evaluator for my Kernel-like language in a few hundred lines of Common Lisp.

# So?

Kernel appeals to me essentially because the behavior of "special operators" is removed from the interpreter or compiler or whatever language processor, and instead isolated in the operative object. That strikes me as a better separation. Of course Lisp implementations often do something similar internally (off the top of my head, SBCL's IR1 translators) but in an ad hoc kind of way.

Continuing this thought leads to some interesting consequences for implementation design.

A conventional Lisp compiler usually has some "specific" transformations: it knows how to rewrite calls to some procedures into more specific forms. For example it might avoid application lists by transforming `(list* z y x)` into `(cons z (cons y x))`, or constant fold `(+ a 2 b 4)` into `(+ a b 6)`. Some of these can be applied even when only some arguments are known - `(apply + a 2 b 4 ls)` into `(apply + 6 a b ls)` is perfectly fine.

Macros and special operators however are all or nothing. The compiler cannot deal with a partial form, at all. A macroexpansion function simply can't work with incomplete data, and the special operator handlers probably can't either.

Most of the time this doesn't matter, because the compiler only receives a complete code sample. But in Lisp, code is data, and sometimes we might want to do something a little exotic.

Take the earlier "tagging" `$cond`. Let's say we would like a function that returns a tag like this for a given object. However, say that when we are writing the program we only new a few of these predicates, while others are to be specified later. Nonetheless the specification happens once while the function will be called many times, so we want it to be fast.

In Common Lisp we could write:

```lisp
(defun tagger (predicates)
  (compile nil `(lambda (object)
                  (cond ((integerp object) 0)
                        ((symbolp object) 1)
                        ,@(loop for pred in predicates for i from 2
                                collect `((,pred object) ,i))))))
```

Now for example `(tagger '(consp characterp))` returns a function that returns 0 for integers, 1 for symbols, 2 for conses, and 3 for characters.

When the definition of `tagger` is compiled, it will (in any Common Lisp I am aware of) not do anything with the constant data. No compilation of the function to be produced will take place until the predicates are actually presented. If `tagger` is called multiple times, each time the compiler will go through the same motions of compiling the first two cond clauses.

Obviously it in some sense does not _need_ to because that stuff is constant, but there is no real way to express this. But I think Kernel may provide a way for thinking about this, which I will explain below.

Again, for most code this doesn't matter. Partial compiling is however genuinely interesting and I think practical for a few purposes - tabular stuff like this mostly. Systems that aggressively JIT compile, like say a CLOS implementation just hypothetically, might benefit.

This is pretty much a completely different reason from anything Dr. Shutt stated by the way. Dr. Shutt was about purity and crystalization (he used literally these terms) whereas I enjoy playing with balls of mud.

# How can you possibly compile this?

It's a thinker, ain't it? If the compiler sees `(f a b c)` with no other information, it is possible `f` is an operative, and will need the dynamic environment and the unevaluated `(a b c)`, and can do literally anything with those things, thus making further optimization essentially impossible. Literally the compiler cannot do _anything_ that would be an improvement on `(eval '(f a b c) env)`, other than noting that this definitely isn't a symbol lookup.

My plan is to use types very aggressively, to develop representation selection mechanisms to be able to avoid consing, and to design the compiler overall as a partial evaluator.

The idea to use types is a no-brainer. If the compiler knows `f` is of type `applicative` (and ignores its dynamic environment, as applicatives almost all do) it can compile this the same as a conventional Lisp compiles any function call. I don't think any especially novel techniques are involved, but there is an urgency to typing that Lisps don't usually have. Some of the details are rather involved; in particular, the type of a first-class environment needs to carry around a typing context for the names it binds (so that for example the type of `(eval 'x env)` can be derived), and operatives need to be tagged with if and how they use their dynamic environment.

Representation selection is a bit less obvious, but important. In some cases the compiler may not be able to totally remove references to the dynamic environment. Initially I thought this was fatal, but now I'm not so sure. A dynamic environment with a fixed set of bindings (see "resembles" below) is just a control frame. If the environment is only used in the dynamic extent of the function producing it, it can just be an actual stack frame (possibly with a small constant vector of names and a link to the parent environment), so no consing whatsoever needs to be done. If it escapes, it's just a heap-allocated frame, as Schemes with continuations do sometimes. No big.

A partial evaluator is I think the only way to make a compiler work. What I mean by "partial evaluator" is that the compiler is able to work on calls to `eval`. If it sees `(eval '(+ a 2 4 b) env)`, it needs to be able to do that constant fold same as the addition had been written literally. If it can do that, suddenly explicit evaluation everywhere becomes very okay. This is just a matter of letting the compiler work on partial data as described above, and doing so becomes conceptually (if not technically) simple in terms of operatives.

# $cond example continued

Here's `tagger` in Kernel. (This isn't idiomatic Kernel, since it uses quasiquotation, but it mirrors the CL definition.)

```lisp
($define! tagger
  ($lambda (predicates)
    (eval `($lambda (object)
             ($cond ((integer? object) 0)
                    ((symbol? object) 1)
                    ,@(map ($lambda (pred i) `((,pred object) ,(+ 2 i)))
                           predicates (iota (length predicates)))))
          (get-current-environment))))
```

Other than `eval` requiring an environment, and using `iota` instead of `loop`, it is pretty much identical. How can a compiler deal with this effectively?

First, it can see that the argument to `eval` is a list, and the car of this list is `$lambda`. If it knows the type of the environment (for example from knowing the environment `tagger` is defined in, as it must), and that environment has `$lambda` bound to the usual, it can understand this to be the definition of a function. It can move the evaluation into the function:

```lisp
($define! tagger
  ($lambda (predicates)
    ($lambda (object)
      (eval `($cond ((integer? object) 0)
                    ((symbol? object) 1)
                    ,@(map ...)) (get-current-environment)))))
```

Already we have a sort of improvement over the CL compiler: it has been determined that `tagger` returns a function of one argument. Argument parsing code can be generated ahead of time, and perhaps functions that call `tagger` can use this type information.

Next it can examine this new form passed to `eval`, and see that it is a `$cond` combination. It can inline `$cond`, unintuitive as that sounds. If it does so (code skipped for brevity), it can constant fold the first two clauses:

```lisp
($define! tagger
  ($lambda (predicates)
    ($lambda (object)
      ($let ((env (get-current-environment)))
        ($if (eval '(integer? object) env)
             (eval '($sequence 0) env)
             ($if (eval '(symbol? object) env)
                  (eval '($sequence 1) env)
                  (eval `($cond ,@(map ...)) env)))))))
```

Other than the recursive `$cond`, the `eval` calls now reduce pretty straightforwardly:

```lisp
($define! tagger
  ($lambda (predicates)
    ($lambda (object)
      ($if (integer? object)
           0
           ($if (symbol? object)
                1
                (eval `($cond ,@(map ...)) (get-current-environment)))))))
```

Ain't that something?

Of course, the resulting taggers are worse than in the CL example, because objects that aren't integers or symbols will end up in `eval`, and `eval` is still not an especially fast way to do things. A really smart compiler could perhaps analyze the `map`, determine the forms of the clauses, and reduce the evaluation to `($let/ec e (map ($lambda (predicate i) ($if (call predicate object) (apply-continuation e (list (+ 2 i))) #f)) predicates (iota (length predicates))))`. But that is rather unlikely. A compiler wouldn't have to be nearly as smart to simply outline:

```lisp
($define! tagger
  ($lambda (predicates)
    ($let ((cont (eval `($lambda (object) ($cond ,@(map ...)))
                       (get-current-environment))))
      ($lambda (object)
        ($if (integer? object)
             0
             ($if (symbol? object)
                  1
                  (call cont object)))))))
```

Aaaaand presto, an efficient use of the partial compiler. Of course many nontrivial details have been glossed over, such as how it knows when to outline, but I think they are surmountable.

```lisp
($define! tagger
  ($lambda (predicates)
    ($lambda (object)
      ($cond ((integer? object) 0)
             ((symbol? object) 1)
             (#t (+ 2 (position object predicates
                                ($lambda (pred) (call pred object)))))))))
```

# You said this "resembles" Kernel?

Congratulations on being one of the like twelve human beings who actually know about Kernel in detail!

Yeah I have a few issues with Kernel's design. The big ones are as follows:

* Environment mutation: In Hominy, you cannot _add bindings to_ environments created by `$vau` (but the toplevel REPL environment and `make-environment` are ok).
  * Modifying existing bindings is one thing. Dr. Shutt went through some effort to avoid chaos - defining `$define!` to not modify bindings in parent environments - in an attempt to keep things reasonable. But he failed, because you can still _add_ bindings all over the dang place. Consider `($lambda (f x) ($sequence (call f x) ($if ...)))`: without further information, the compiler must assume that `f` could add a local binding for `$if` to the lambda environment, and so it couldn't compile the `$if` form at all. Ridiculous. Dr. Shutt lifted Scheme's "define as short for letrec" thing into doing actual mutation, but geez, letrec is just so much cleaner.
* Circular lists: I don't care. Supporting circular lists in `$let` or something is weird but okay. But the amount of effort put into `map` and `reduce` for example is just silly.
* Continuations: I do intend to implement some kind of continuations eventually; at the very least escape continuations, maybe delimited continuations since they're interesting, maybe full on `call/cc`.
  * The `guard-continuation` thing is interesting, but I don't think it's really practical to represent error behavior. Conceptually, we sorta send all errors of different types to one continuation - the debugger. When we want some error to be handled in some extent, we say that, within that extent, some kind of error should go to some different continuation instead. We don't define a different continuation for every kind of error.
