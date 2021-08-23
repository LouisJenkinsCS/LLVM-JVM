# LLVM-JVM

**Code for Functional Bytecode Interpreter (Proof-of-Concept) has been moved here: https://github.com/LouisJenkinsCS/Functional-JVM-Bytecode-Interpreter

LLVM-JVM is an attempt at creating a Java Virtual Machine with Just-In-Time compilation
that uses LLVM as the backend, similar to [Azul System's](https://www.azul.com/called-new-jit-compiler-falcon/).
This project, however, is an educational one and likely will never be production-ready.
This project has been funded by Bloomsburg University's Professional Experience Grant (PEG).

## Note


**Warning:** The code is a complete and utter mess! You've been warned!

LLVM-JVM uses a ripped and modified version of [MateVM](https://github.com/MateVM/MateVM)'s runtime, while
supplying its own frontend to LLVM. As such, everything contained in the 'MateVMRuntime'
folder is code that belongs to the original authors, and I do not take credit for anything
other than the modest changes I make, if any.

### Why not just fork the MateVM project?

The MateVM project has been inactive for ~5 years, and considering that I would need to
strip a lot of it away, such as Hoopl and Harpy, I would just take the best parts of
them and give proper attribution. This not only saves me considerable time, which
is something I have very little of, but also allows me to have access to the issue-tracker
as well as other things.

## Progress

Note: the criteria for progress is bars minimum, such as basic integer arithmetic and if-else statements. If it is checked it does not mean support for all of it, just that the ground work is done and can easily be extended on.

- [X] Basic Arithmetic
  - [X] Integer
  - [ ] Float
  - [ ] Double
  - [ ] Long
- [X] Basic Control Flow
  - [X] Loops
  - [X] Conditionals
- [X] LLVM optimization passes
- [X] Global Variables
  - [X] Static Fields
  - [ ] Instance Fields
- [ ] Dynamic ClassLoading
  - [ ] JIT at Runtime
- [ ] Garbage Collection
  - [ ] Boehms

## Building

**Requirements:**
1. LLVM w/ llvm-config
2. Haskell
3. Java7+ (for GUI)


To build, run 'cabal install' and 'ghc Main.hs -o Main.exe', then 'java -jar GUI/llvm-jvm-frontend.jar'
if you desire the GUI.
