#!/bin/bash
ocamlc -c simpltypes.ml
ocamlyacc simplparser.mly
ocamlc -c simplparser.mli
ocamlc -c simplparser.ml
ocamllex simpllexer.mll
ocamlc -c simpllexer.ml
ocamlc -c nazneen.ml
ocamlc -o simpl simpltypes.cmo simpllexer.cmo simplparser.cmo nazneen.cmo
