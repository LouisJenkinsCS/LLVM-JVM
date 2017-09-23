# LLVM-JVM

## Note

LLVM-JVM uses a ripped and modified version of [MateVM](https://github.com/MateVM/MateVM)'s runtime, while
supplying its own frontend and backend to LLVM. As such, everything
contained in the 'MateVMRuntime' folder is code that originally belongs
to the original authors.

## What is it?

A Java Virtual Machine written in Haskell. The planned outcome is adding JIT compilation that will compile JVM bytecode to LLVM bitcode.
