A virtual machine provides a concrete but malleable target which should be suitable for writing the kinds of optimizations a working Kernel compiler needs to have - stuff like recognizing applicatives that's more-or-less machine independent. Despite being totally artificial, a VM is a more natural and less finicky target that the CL compilation thing used previously.

With the VM done, I want to write three compilers. The first is a simple single-pass compiler that just does a basic translation into bytecode. Though this rules out some important optimizations (most obviously, not consing an environment), several important optimizations should still be possible. So the single-pass compiler can be a concrete testbed for ironing out what kind of information about values we need.

Next is a two-pass compiler that first converts the code into a tree-based IR, and then lowers that into bytecode. This allows more sophisticated but critical optimizations like closure reduction.

The third and final compiler will be the one I've already put a lot of effort into - a proper IR with explicit control flow via CPS. I think starting out with this one was a mistake, because it's not really possible to get it going without extra work, and what good is that work for? Flow should only be an advantage over the tree compiler when we want to analyze what arguments to local functions are, which I suspect will be marginal compared to what the tree compiler should be able to do.

All these compilers can share info structures, compiler environments, the VM definition, assembler, disassembler, and linking. This last is important - once I get something actually working with the single-pass or tree compilers, I can think about how I want linking to work. This also applies to the compiler environments.