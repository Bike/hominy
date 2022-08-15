This document collects my thoughts on how a better `compile-file` ought to work. It is not a formal specification, but may hopefully be turned into one in the future.

First, I give a rigorous and detailed idea of what compilation and file compilation conceptually are or at least can be (compilation is partial evaluation, and compile-file is describing a module and serializing a module). Then I elaborate on interfaces to those systems. And finally wrap it up back into `compile-file`.

# Modules

"module" is an informal term for an environment that presents a useful interface. As such there are no distinct module objects in Burke.

However, for convenient specification of environments-as-modules, the `$module` operative is provided. `($module exports . bindings`) evaluates to an environment with the symbols in the exports list, and only those symbols, bound. The values are provided from the bindings, which are evaluated in the dynamic environment and may recursively refer to each other as in `$letrec`. The bindings may include symbols not in the exports list, which will be available for producing the module but will not be visible in the resulting environment.

`$module-redirect` and `$module-safe` are operatives in the same family allowing convenient specification of an environment for the bindings to be executed in (a provided environment or a standard environment, respectively).

Another way of defining modules is the `$cmodule` family, described below as part of the compiler.

# Compilation

Compilation is the process of converting an operative into a more efficient form, based on information about that operative's static environment. It is a kind of partial evaluation. After compilation, an operative may, at the discretion of the implementation, become insensitive to future changes in its static environment, as long as such insensitivity is allowed by the assumptions given to the compiler.

## Compilation Environments

Assumptions about an environment are stored in a _compilation environment_. This is an object satisfying the `compilation-environment?` predicate. Compilation environments are similar enough to environments to share the term, but are not actually environments (i.e. `environment?` on them is false). A compilation environment contains a set of bindings and has a list of parents, like an environment, and lookup (with `lookup-info`) proceeds in a similar way. Compilation environments are also _complete_ or _incomplete_: completeness indicates that all bindings the corresponding environment could have are accounted for in the compilation environment (i.e. additional bindings are impossible). Completeness does not affect parents, i.e. a complete compilation environment can have incomplete parents, and vice versa, although the parents of an incomplete compilation environment generally cannot be used to provide any information.

Binding presence works a bit differently for compilation environments than for environments. If a compilation environment does not include a binding for a symbol, looking up that symbol will will still return an info (e.g. a `top-info`). `lookup-binding` can be used to query the runtime status of a binding. It returns an indicator (a symbol maybe?): either DEFINITELY-BOUND, MAYBE-BOUND, or NOT-BOUND. The former means that the compiler may assume that the symbol will be bound, the last that it can assume it won't be, and MAYBE-BOUND means neither can be assumed.

Infos and compilation environments may be constructed manually through operatives I haven't invented yet, but in practice, programmers will usually higher level operatives to produce them, or may not even deal with them directly. Compilation environments are the regular structure underlying these operatives.

A `standard-compilation-environment` is provided, that can be used to compile operatives with the assumption that all the standard symbols will be bound to their standard values.

### Info

Instead of values, a compilation environment contains "infos" (`info?`), each of which is information about the properties the compiler is allowed to assume about a value. `top-info` indicates the value can be anything, i.e. the compiler can assume nothing. At the other extreme, a `constant-info` indicates that at runtime, the value will be _similar_ (see below) to the value stored in the info (via `constant-info-value`).

Infos can also contain just types. TODO.

### Operations on Compilation Environments

`meet-compilation-environments` takes any number of compilation environments and returns a compilation environment including all of the assumptions of all of the environments. Specifically, the resulting compilation environment includes all bindings in any of the input environments, and the infos for any bindings present in multiple inputs are as produced by `meet-infos`.

## Compilation Semantics

Compiled operatives behave identically to their interpreted counterparts, i.e. with the usual language semantics, as long as the compilation environment's assumptions hold true for the operative's static environent. They are functionally indistinguishable, and compilation is transparent.

If the compilation environment's assumptions no longer hold, the effects are undefined in unsafe code. For example, this could happen if a symbol was incompatibly redefined. In safe code, either the program behaves as though the old compatible definition was in place, as if the new definition was in place, or it signals an error.

## Run-time compilation

The basic interface to the compiler is the `compile` applicative. `compile` accepts two arguments, a combiner and a compilation environment, and returns a compiled combiner. This compiler may be `eq?` to the input combiner, but `compile` is not required to perform any side effects to t he input combiner or otherwise. If the input combiner is already the result of a compilation, the effect is as if the original combiner was compiled in the meet of all the compilation environments it and its subsequent compilations have been compiled in.

`compiled-operative` is defined as though it builds an operative normally (with `$vau`) and then compiles it in the given compilation environment, i.e. `(compiled-operative cenv static-env plist eparam body)` = `(compile (eval (list* $vau plist eparam body) static-env) cenv)`. It is provided for convenience, and for efficiency as the implementation may in this circumstance run the compiler directly without producing an intermediate and immediately discarded interpreted operative.

## Local Compilation Environments

There should be a way to interleave normal code with notes only relevant to the compiler, as with `cl:declare`. I have not considered this in detail yet.

## Macros

A _macro_ is a particular kind of operative that can be processed easily and efficiently by the compiler. Macro operatives have the general form: `($vau combinand dynenv (eval ((wrap [macroexpander]) combinand dynenv) dynenv))`. In other words, their action consists of producing some form based on their combinand and dynenv (the "macroexpander" form), and evaluating that form in the current dynenv. Assuming the macroexpander does not have side effects, macro forms can be compiled by simply inserting the produced form in place of the original.

Macros are operatives (`operative?` is true) as well as macros (`macro?`). The underlying _macro expander_ for a macro can be retrieved with `macroexpander`.

Macros can be produced using the `$macro` operative, or at a lower level with the `macro` applicative. `(macro expander)` produces a macro that uses `expander`, a combiner, as its macroexpander. `($macro combinand dynenv form)` produces a macro which evaluates the `form` to produce its expansion. `($macro combinand dynenv form)` = `(macro ($vau (combinand dynenv) #ignore form))`.

Semantically, the difference between a macro and the equivalent non-macro operative is that macroexpansion may take place any number of times, and at any time. That is, a conforming program cannot expect that a macro is only expanded once when it is combined, whereas it can with the non-macro. This is the case regardless of whether a macro form is being evaluated or compiled. This means that a conforming program should not rely on macros to perform side effects at any particular time. Ideally, macroexpanders should be pure functions.

The compiler can only take advantage of macroexpansions if it knows about them, i.e. if the compilation environment includes the macro somehow, as by in a `constant-info`.

(The semantics of macros in evaluated and compiled code are identical to promote the transparent compilation semantics. I don't think minimal compilation in the CL sense is actually all that useful in practice except I guess as motivation to make the compiler actually compile, but like, who puts side effects in macros anyway? Also, expanding all macros is impossible in Burke. `load-time-value` is genuinely different, but pretty marginal.)

## Compile Time Values

The operative `$compile-time-value` is treated differently by the evaluator and the compiler. To the evaluator, it is almost an identity: it could be defined by `($vau (form) #ignore (eval form (make-standard-environment)))`. The compiler (that is, with its standard info) however, may evaluate the form in a standard environment at compile time, and substitute the resulting value for the form.

(I think Common Lisp's `load-time-value` versus `#.` isn't relevant to Burke, since essentially all objects should be serializable, and if you want side effects at load time please reconsider and also load-time-value is inappropriate. Sometimes people do use it like "static" locals in C - might be worth considering, I guess.)

## Module Specification

In practice, it is convenient to be able to be able to specify a module and a corresponding compilation environment in concert, to use a specified compilation environment for compiling the module itself, and to compile all code in a module at once rather than to go through every operative individually.

This specification in Burke takes the form of a list of _directives_. A directive resembles a form, but is headed by a `_directive specifier_ rather than a combiner. `$cmodule` (or `directives->module` as an applicative, or `load-module` to read directives from a file) then interprets the directives to produce a module and compilation environment.

Directives are of type `directive?` and include e.g. `define`, `inline`, `defmacro`, `directive-macrolet`, `define-directive-macro`, something something more later.

## Inheritance

Like, importing modules. Having a module specify its requirements. TODO

# Marshaling

With few exceptions (continuations are the only standard thing I can think of, but that could be doable), all objects in Burke can be _marshaled_ and then _unmarshaled_ to form _similar_ objects. Marshaling is the process of serializing objects into sequences which can be e.g. written to files, and unmarshaling the process of deserializing them back into objects. The standard requires a marshaling format using eight-bit bytes, but other formats may be provided by implementations.

Marshaling and unmarshaling can be modified by linking. Linking allows some objects accessible in the original marshaling context to be replaced in the later unmarshaled object by counterparts in the unmarshaling context, outside the unmarshaled object.

## Similarity

In order to rigorously define what marshaling does, a _similarity_ predicate is defined. Similarity is not an accessible applicative because it may apply to objects in different Burke images, and is in general uncomputable as described below. Unmarshaling a marshaled object produces an object that is _similar_ to the original, but not necessarily `eq?` or `equal?`, even if these processes took place in the same image. Similarity is intended to be strictly defined enough that most uses of an object will behave identically, and roughly speaking, similar objects will be about `equal?`.

(Kernel defines `equal?` in terms of external representations. In my view, this is lazy and should not be done.)

### Definition of Similarity

Booleans are similar iff they have the same truth value. `#inert` is similar only to all inert objects, and `#ignore` similar only to all ignore objects.

Symbols are similar iff they have the same name.

Conses are similar iff their cars are similar and their cdrs are similar, and iff they are either both immutable or both mutable.

Operatives are similar if they have the same results and side effects on all possible inputs in all program states. This is obviously uncomputable, which is one reason there is no `similar?`; but marshaling and unmarshaling never needs to **check** similarity of objects, merely to produce similar objects to those given, so a requirement of similar operatives is implementable.

Applicatives are similar if their underlying combiners are similar.

Environments are similar iff the following conditions are met:

* they bind the same set of symbols
* the values bound to these symbols are similar
* the environments are all mutable or all immutable

Note that the inheritance structure of an environment does not need to be preserved

Continuations I'll leave for later.

Note that the unmarshaler is permitted by the general as-if rule to weaken these conditions if the consequences are unobservable. For example, if an operative is marshaled, and closes over a cons that is not externally accessible and cannot be passed to `set-car!` or `set-cdr!`, that cons could be made immutable even if it was not originally. A marshaled applicative could end up as an operative (or vice versa) if it is only ever combined, and not passed to `applicative?` or `operative?`. A marshaled environment could have inaccessible bindings removed.

## Substructure Preservation

The marshaler and unmarshaler are required to preserve the identity of subobjects of aggregate objects relative to one another. That is, if two parts of a marshaled object are `eq?`, their similar counterparts in a later unmarshaled object must also be `eq?`. There is no requirement that they be `eq?` to the originals.

## Linking

Marshaling and unmarshaling take place relative to a _linkage environment_. This is simply an environment (a normal environment meeting `environment?`) that the programmer or higher level operatives provide to the marshaler and unmarshaler.

When the marshaler encounters an object, and that object is either a value bound by the linkage environment or the linkage environment itself, that object is not marshaled normally. Instead, the marshaler indicates the linkage in some way to the unmarshaler. When the unmarshaler encounters that indication, rather than construct an object from the marshaling, it looks up the object in its linkage environment and incorporates that.

For example, say `(cons 4 $if)` is evaluated in a standard environment, and marshaled relative to a standard environment. If this is unmarshaled relative to a standard environment, the cdr of the resulting object will be `eq?` to `$if`, even if the unmarshaling takes place in a completely different image.

### Linking Safety

It is possible for an operative to be compiled relative to some compilation environment, to be subsequently marshaled with respect to the corresponding environment, and then to be unmarshaled with respect to an environment that does not meet the assumptions of the compilation environment. More generally, the unmarshaling environment may not be _similar_ to the marshaling environment. In general, it is the responsibility of the programmer to prevent this, and the consequences are undefined if they don't.

The unmarshaler should signal a warning if it can determine that the unmarshaling environment is not similar to the marshaling environment, and a style warning if it can't determine that they are similar. Maybe?

### Linking and Compilation

Semantically, operatives contain references to their static environments. It is likely that a sufficiently compiled operative will not, but for linking purposes it must behave as if it does. For example, say `($vau () #ignore x)` is evaluated in some environment containing x and subsequently compiled. This may be implemented as a closure over the value of x, or for a mutable environment, a cell containing x's value. If this operative is then marshaled and unmarshaled relative to an environment containing x, the unmarshaled operative must remain sensitive to changes to the binding of x, unless the compilation environment permitted it not to be.

In contrast, we could have the operative `(eval (list $vau () #ignore x) (make-standard-environment))`. If marshaled and unmarshaled relative to an environment containing x, this operator would return whatever value x had at unmarshaling time (because that value was in a binding in the linkage environment, and thus subject to linking) but would not be sensitive to changes to x's binding, because semantically the operative just returns the object in the body it was evaluated with, rather than looking up the variable x.

### Technical Note

Shutt's notes on `$binds?` mention that the programmer should be unable to get a complete list of variables bound by an environment, but that the capability to enumerate such a list is not a problem. The requirements of linkage allow programmers to determine whether a given **value** is bound to some variable in an environment, by careful use of marshaling and unmarshaling using that environment as the linkage environment. But similarly to `$binds?`, this would only allow an enumeration procedure.

# Putting It All Together

`compile-file` is then the following series of operations:

1. Read in a file (note: there could be special directives for readtable stuff, that would have to be executed sequentially while reading) as a sequence of directives
2. Interpret those directives into a module
3. Marshal that module to a byte stream
4 ...outputting to a FASL file.
