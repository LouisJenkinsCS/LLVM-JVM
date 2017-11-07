# LLVM-JVM

README under construction...

**Warning:** The code is a complete and utter mess! You've been warned!

## Note

LLVM-JVM uses a ripped and modified version of [MateVM](https://github.com/MateVM/MateVM)'s runtime, while
supplying its own frontend and backend to LLVM. As such, everything
contained in the 'MateVMRuntime' folder is code that originally belongs
to the original authors.

### Why not just fork the MateVM project?

Originally the idea was to only use a smaller subset of the MateVM project, as I had no need for Harpy or Hoopl (as I will be using LLVM), essentially, with the goal of saving time. I decided to take the easy route of taking what I needed and giving proper contribution, since MateVM project is over 5 years old and hasn't been touched since, and to get the project to build requires non-trivial effort as parts of it and its dependencies no longer build appropriately. 

## What is it?

A Java Virtual Machine with the goals of providing a frontend to convert JVM bytecode to LLVM IR. Inspired by Azul Zing JVM and their Falcon JIT. For an Independent Study (with a modest research grant [Not NSF])

## Progress

Note: the criteria for progress is bars minimum, such as basic integer arithmetic and if-else statements. If it is checked it does not mean support for all of it, just that the ground work is done and can easily be extended on. 

- [X] Basic Arithmetic
- [X] Basic Control Flow
- [X] LLVM optimization passes
- [X] Global Variables (Fields)
- [ ] Dynamic ClassLoading (JIT at runtime)
- [ ] Garbage Collection (Boehms)
