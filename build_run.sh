ocamlbuild -I data -I src -package dyp -pkg ppx_deriving.show -use-ocamlfind src/main.native
./main.native
