A virtual machine provides a concrete but malleable target which should be suitable for writing the kinds of optimizations a working Kernel compiler needs to have - stuff like recognizing applicatives that's more-or-less machine independent. Despite being totally artificial, a VM is a more natural and less finicky target that the CL compilation thing used previously.

With the VM done, I want to write three compilers. The first is a simple single-pass compiler that just does a basic translation into bytecode. Though this rules out some important optimizations (most obviously, not consing an environment), several important optimizations should still be possible. So the single-pass compiler can be a concrete testbed for ironing out what kind of information about values we need.

Next is a two-pass compiler that first converts the code into a tree-based IR, and then lowers that into bytecode. This allows more sophisticated but critical optimizations like closure reduction.

The third and final compiler will be the one I've already put a lot of effort into - a proper IR with explicit control flow via CPS. I think starting out with this one was a mistake, because it's not really possible to get it going without extra work, and what good is that work for? Flow should only be an advantage over the tree compiler when we want to analyze what arguments to local functions are, which I suspect will be marginal compared to what the tree compiler should be able to do. (It also matters when mutating variables is possible!)

All these compilers can share info structures, compiler environments, the VM definition, assembler, disassembler, and linking. This last is important - once I get something actually working with the single-pass or tree compilers, I can think about how I want linking to work. This also applies to the compiler environments.

Now treec is working on basic code. I do still want to ultimately do the flow compiler, but I should also think about what I want to do and what I need to do it.

My fundamental goal is to have a testbed language with very simple language semantics that I can use to facilitate my thinking about compilers and programming languages. To this end I would actually like something that could be usable as a daily driver, because I think the simple abstractions alone are not enough to understand how a programming language can be used - as a basic example, I have Burke "working" as I write this, but there is no arithmetic: a subsystem that most serious programs would involve (for example, the VM and compilers I have written in CL do), and a subsystem whose optimization is a vastly complex problem on its own.

Anyway, quick TODO list, in no order:

- language
-- X core
--- X vau abstraction
---- X ptrees
--- X applicatives
--- X environments
-- mutation
--- iron out semantics
--- implement
-- I/O ports
--- design semantics
--- implement
-- arithmetic
--- report's semantics should be fine, but check them
--- implement
-- vectors
--- design semantics
--- typed vectors (good for bytevectors which are good for marshaling)
--- implement
-- marshaling
--- X basic implementation
--- finish + test semantics related to linking
--- expose to burke
-- generic functions
--- design semantics
--- make eval/lookup/combine generic?
-- continuations
--- escape/abort
--- delimited
--- guards
--- marks?
--- prompts?
-- conditions
--- syntax/operatives
--- hierarchy
-- dynamic binding
-- X static binding (i.e. the report's "gensyms")
--- convenient macrology (e.g. once-only)
-- X macros
--- X expose to Burke
-- types
-- type definition
--- generalize encapsulated types to hold multiple objects directly (more efficient i hope)
- conforming kernel option
-- circularity
- compilation semantics
-- X draft
-- evaluate how well the compilation environment thing works
-- and what interface for making them is best
-- local compilation environments (e.g. type declarations)
-- macroexpansion
- modules
-- directives & syntax
-- implement
-- write standard library as module(s)?
- running offline
-- C linking
-- ABI
-- native VM
- dynamic contexts
-- figure out what the hell
-- implement
-- integrate with generic functions
-- integrate with compilation environment?
- VM
-- X basic design
-- X implement
-- variable size jumps
-- variable size operands
-- explicit frame representation for continuations
-- better argument representation for applicatives?
- assembler
-- X write
-- X forward labels
-- labels across functions
-- backward jumps
-- labels for entry points
-- X disassembler
--- resolve labels
--- more human readable output
- treec
-- X core operation
-- X basic operatives
-- X vau forms (i.e. literal $vau in head)
--- with wrapping
-- X inline applicatives with VM equivalents
-- $letrec
-- direct calls
-- real tail calls
- flowc
-- X IR design
-- X core operation
-- reorient myself
-- known operators
-- use local transformations (i.e. rewrites that only replace callees and then inline them)
-- other optimizations, e.g. LICM
-- optimize in parallel (callee transforms)
