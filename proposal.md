# Java Virtual Machine (Interpreter) in Haskell

### **Author**: Louis Jenkins

### **Project Title:** Java Virtual Machine (Interpreter)

### **Language:** Haskell

### **Compiler:** GHC

## Project Summary

### JVM Initialization

Implementing a Java Virtual Machine to interpret a subset of core ByteCode instructions. The program will begin by accepting a single `.class` file, generated using `javac` (and hence requires the Java Runtime Environment),
wherein it will be parsed into it's respective data structure representation, as represented in §4.1 in the JVM Specification. After constructing the `.class` file, it will construct the method area (§2.5.4) based on
`.class` file's constant pool, method and field descriptors, as well as relevant attributes. The runtime stack (§2.5.2) and it's initial `Frame` (§2.6) will be constructed based on the `Code` (§4.7.3) attribute attached
to the `static void main(...)` method, which serves as the entry point for the program. The `Heap` (§2.5.3) constructed will unfortunately not be garbage collected, and will deallocate heap objects when the `Frame` they
are allocated on is popped. As well, the steps to linking the runtime (`java/lang/*`, `java/net/*`, `java/stream/*`, etc.) will be skipped, as it would require a complete implementation of the Bytecode instructions.

### JVM Runtime

Beginning by executing all ByteCode instructions supplied in `static void main(...)` that does not invoke instructions currently not implemented, it will successfully run from beginning to completion. Control constructs
such as `if...else if...else`, `for`, `while`, and `switch` (example seen in §3.2 & §3.5) should be implemented, and handled appropriately, as well as basic IO, such as `System.out.println`, which will be substituted for
the built-in Haskell functionality. As well, String concatenation of primitive types and `String` type will be supported (as they are generally created via a `StringBuilder`, wherein constants are held int he Constant Pool,
example of accessing constants can be seen at §3.4), as they can be further substituted for built-in types. As well, auto-boxed objects such as `Integer`, `Double`, and `Float` (auto-boxing generally created using the
`valueOf` static method) can also be substituted. As well, support for invoking methods (creating a new stack frame) as well as basic exception handling (only `java/lang/Exception`), the latter being optional by the time
of presentation, should simulate the stack.
