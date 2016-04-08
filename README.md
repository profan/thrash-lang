thrash
---------------
A small language project for a thing which is intended to compile to bash, mostly because I hate myself and also because I needed some practice writing OCaml and working with dypgen. It will be a dynamic language like bash and mostly mimic it's semantics with the exception of some eq/= etc oddities and various other things like global by default variables which make me hate bash and my life.

This will probably end up being a horrific melting pot of features over time.
Also, using dypgen for this project is probably not necessary, lets consider menhir as LR probably handles our language fine as I think it's free of ambiguities.

Probably put the grammar in not-parser-generator form somewhere later.

As an important side note, **no-one** should **ever** use this.

Example
-----------
```
let somevar = 125
let somebool = true

if true do
    25
else
    15
end

for x in [1, 2, 3, 4] do
    65
end

while 10 do
    if 25 do
        35
    else
        45
    end
end

if x = 25 do
    30
else
    25
end
```

Structure
-----------
* **src** folder holds source files for the compiler, currently just the main which parses and evaluates the single test source file.
* **data** folder holds data definitions used to generate the parser and lexer.
* **tests** folder holds samples to be used for testing the compiler.

License
-------
GNU GPLv3
