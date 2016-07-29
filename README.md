Handmade Language
=================

A toy programming language compiler with LLVM backend:
 - The language is relatively low level - similar to C. It is imperative and supports:
    - procedural programming,
    - user defined data structures,
    - localy defined data structures and procedures,
    - basic type inference and more.
 - Mostly clean design: separate tokenizer, parser, semantic analysis and LLVM code generation middleware. Also code gen part builds as a separate dynamic library so in theory it should be easy to implement a different code generator based off of the same AST.
 - The code for the compiler is C-style with a bit of C++'isms here and there. I wrote it on a Mac and never ran it on other platforms. It should be mostly portable though.
 - The project contains an example on how to link hmlang program from C as a dynamic library. Actually the language itself doesn't include any standard library so you have to use C or asm if you want any side-effects (like printing) to occure.

See example.hm for some examples in hmlang itself. Try shell scripts included to build the compiler and compile/execute the example.

There are a lot of missing functionality and some interesting work-in-progress features (e.g. compile time code execution). I think the project serves as a good mostly complete example on how to make a compiler and use LLVM to generate native code. The 'Handmade' in the name is a tribute to [Handmade Hero](https://handmadehero.org/) project. The work was also inspired by [Jai](https://www.youtube.com/watch?v=TH9VCN6UkyQ&list=PLmV5I2fxaiCKfxMBrNsU1kgKJXD3PkyxO) programming language.
