thrash
---------------
A small language project for a thing which is intended to compile to bash, mostly because I hate myself and also because I needed some practice writing OCaml and working with dypgen. It will be a dynamic language like bash and mostly mimic it's semantics with the exception of some eq/= etc oddities and various other things like global by default variables which make me hate bash and my life.

This will probably end up being a horrific melting pot of features over time.

Structure
-----------
* **src** folder holds source files for the compiler, currently just the main which parses and evaluates the single test source file.
* **data** folder holds data definitions used to generate the parser and lexer.
* **tests** folder holds samples to be used for testing the compiler.

License
-------
GNU GPLv3
