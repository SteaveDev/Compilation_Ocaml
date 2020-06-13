#!/bin/bash
ocamlc -c AST.ml
ocamllex lexeur.mll
ocamlyacc parseur.mly
ocamlc -c parseur.mli
ocamlc -c lexeur.ml
ocamlc -c parseur.ml
ocamlc -c main.ml
ocamlc -o start str.cma AST.cmo lexeur.cmo parseur.cmo main.cmo

rm AST.cmi AST.cmo lexeur.cmi lexeur.cmo lexeur.ml parseur.cmi parseur.cmo parseur.ml parseur.mli main.cmi main.cmo