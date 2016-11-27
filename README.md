# Minimal-JVM

## What is it?

A 'toy' JVM interpreter written in Haskell. Currently, it is considered a 'toy' in that it is not
the intention of the author for this to be used as a viable JVM compared to others such as Hotspot
or Oracle's implementations. This is being developed purely for educational purposes, and as such,
I may or may not diverge from the JVM specification.

## Why Haskell

Mind you that this is purely educational, and while I do admit that Haskell may NOT be the best
language for an interpreter, I chose Haskell because I love challenges. Haskell is infamous for
being difficult to learn, and so I figured it would be fun to do so.

There is no particular logical reasons for as to why I chose Haskell beyond this. However, I would
say that Haskell would definitely be better for a JIT Compilation virtual machine instead of an
interpreter, and as such I plan to make those overhauls in the future (given the time).

## How did this project come to be?

This project originally was for a computer science course, called the "Organization of Programming Languages",
and so I figured, why not knock out two birds with one stone by not only teaching myself a new language, a new
paradigm, and to learn more about how languages are implemented?

## Progress

### Class Files

- [x] Parse `.class` file into data structure representation
- [x] Implement support for minimal attributes, (I.E: `Code` attribute)
- [ ] Implement support for all attributes

### Runtime Environment

- [x] Create a stack frame representation
- [ ] Create a heap with minimal garbage collection
- [ ] Implement bootstrap class loader for runtime

### ByteCode

- [x] Implement load and store operations
- [x] Implement arithmetic operations
- [x] Implement conditionals + `goto` jumps
- [ ] Implement all instructions

## Misc. Details

The Virtual Machine implements small stubs for certain method calls that simulate
the runtime, such as `System.out.println`, and as well as appending to a `StringBuilder`.
