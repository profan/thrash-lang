ocamlbuild -I data -I src -package dyp -use-ocamlfind src/main.native
./main.native
