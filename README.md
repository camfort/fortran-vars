# fortran-vars
`fortran-vars` is a static analysis library for Fortran code. It is built on top of the open source project [`fortran-src`](https://github.com/camfort/fortran-src) which provides lexing, parsing and basic analyses of Fortran code. `fortran-vars` focuses on supporting the Fortran 77 standard and extensions. It provides a Fortran memory model with a symbol table and storage table, constant expressions evaluation, constant propagation analysis

## Fortran Memory Model
`fortran-vars` provides a memory model that specifies the allocation arrangement of variables and arrays.

An accurate memory model is fundamental to Fortran static analysis especially for Fortran 77 code. The equivalence statement in Fortran establishes memory association among different symbols. Therefore symbols that refer to the same memory location shall be treated as the same entity in the static analysis, this is especially true for data-flow analysis that tracks the state of variables and arrays.

The usage of equivalence is prevalent in Fortran 77 code. The Fortran 77 standard does not have struct type. A common design pattern that emerged in Fortran 77 code is to mimic a struct type by declaring a large character array and variables of various types, and establishing equivalences among the variables and the character array elements. In effect the character array defines the raw buffer of a struct and the variables become the struct members.

`fortran-vars` assigns each variable or array a memory location. A memory location comprises of a memory block and the offset within that memory block. A memory block is a contiguous block of bytes, and can host multiple variables or arrays provided that they are associated via equivalence. An algorithm inspired by union-find is designed to accurately lay out the offsets of variables and arrays inside a memory block. Each memory block is identified by a unique name, which is conventionally set to the name of the first variable, whose offset is 0, in the memory block. If the memory block contains common variables then the name of the memory block is set to the name of the common area.

The memory model is described with two data structures: symbol table and storage table.

### Symbol Table
Four kinds of symbols are collected in the symbol table. They are parameters, variables, arrays and dummy arguments. The symbol entry contains the data type of the symbol and specific information related to the kind.

* Parameter is named constant whose value is known at compile time. `fortran-vars` evaluates the value of parameter and stores the value in the parameter entry.

* Variable is allocated to a piece of memory based on the memroy model described above. The memory location -- comprised of the name of memory block and the offset -- is recorded in the entry of variable.

* Array is allocated to a piece of memory as well. The memory location of the first element of array is recorded in the array entry.

* Dummy arguments are the arguments used in the definition of a function or subroutine. As Fortran 77 is pass-by-reference for function and subroutine calls, the dummy arguments point to the actual arguments from caller at run time. Therefore, there is no fixed memory location for dummy argument at compile time.

### Storage Table
The storage table contains the metadata of memory blocks. The metadata specifies the size of the memory block, its storage class, and the list of variables and arrays located in the block.

## Constant Expression Evaluation
The `Eval` module provides an interpreter for Fortran constant expressions. Arithmetic, logical, relational, and character expressions are supported. When `fortran-vars` constructs the symbol table, it uses the interpreter to calculate the values of parameters and saves the values in the parameter entries. For a constant expression with parameters, the interpreter looks up the parameter values in the symbol table. Array dimensions are commonly specified with parameters in which case they are determined using the interpreter.

## Constant Propagation Analysis
The constant expression interpreter cannot evaluate expressions with variables or array elements, whose values change at run time. Constant propagation analysis is a data flow analysis that discovers constant values by examining all possible execution paths in the control flow graph of a program and propagating the constants further. The `ConstantPropagation` module provides an expression evaluator based on the constant propagation analysis. For any expression in a program unit, the evaluator determines whether it is a constant, unknown (`Bot`), or uninitialized (`Top`).

The three kinds of values form a three-level lattice with `Top` being the highest of the lattice, `Bot` being the lowest, and all the constant values being in the middle, with no constant value being higher or lower than any other constant.

Variables and array elements are resolved to range of memory by referencing the symbol table and storage table, therefore the states of the constant propagation are represented by mapping from memory ranges to three-level lattice values.

## Usage
`fortran-vars` is mainly designed as a library of static analysis and refactoring of Fortran code. It also comes with a
command-line tool that dumps the symbol table and storage table of the input program in JSON format.

```
fortran-vars (-v|--fortranVersion VERSION) [-I|--include DIRECTORY] FILE
```

## Build
`fortran-vars` uses [`stack`](https://docs.haskellstack.org) for development. To build and test, run:

```
stack build
stack test
```

Alternatively, you can use Cabal:

```
cabal build
cabal test
```

## Contributors
Thanks to the original package authors:

  * Aiden Jeffrey <ajeffrey15@bloomberg.net>
  * Anthony Burzillo <aburzillo@bloomberg.net>
  * Azeem Bande-ali <abandeali@bloomberg.net>
  * Benjamin Groh <bgroh2@bloomberg.net>
  * Chris Cotter <ccotter14@bloomberg.net>
  * Daniel Beer <dbeer1@bloomberg.net>
  * Darius Makovsky <bapophis@bloomberg.net>
  * Eric Schneider <eschneider47@bloomberg.net>
  * Eric Seidel <eseidel13@bloomberg.net>
  * Jason Xu <jxu116@bloomberg.net>
  * Kojo Adams <kadams85@bloomberg.net>
  * Lukasz Kolodziejczyk <lkolodziejc1@bloomberg.net>
  * Mario Longobardi <mlongobardi2@bloomberg.net>
  * Poppy Singleton-Hoare <psingletonho@bloomberg.net>
  * Raoul Hidalgo Charman <rhidalgochar@bloomberg.net>
  * Ti Liang <tliang54@bloomberg.net>
