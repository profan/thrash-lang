#!/usr/bin/env bash

sudo apt-get update
sudo apt-get install -y pkg-config opam

# set up environment
opam init -y

# compile ocaml
opam switch -y 4.02.1
echo "eval \`opam config env\`" >> ~/.bashrc
eval `opam config env`

# get deps
opam install -y batteries core ocamlfind dypgen
