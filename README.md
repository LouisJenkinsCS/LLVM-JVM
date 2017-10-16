# LLVM-JVM

## Note

LLVM-JVM uses a ripped and modified version of [MateVM](https://github.com/MateVM/MateVM)'s runtime, while
supplying its own frontend and backend to LLVM. As such, everything
contained in the 'MateVMRuntime' folder is code that originally belongs
to the original authors.

## What is it?

A Java Virtual Machine with the goals of providing a frontend to convert JVM bytecode to LLVM IR. Inspired by Azul Zing JVM and their Falcon JIT. For an Independent Study (with a modest research grant)
