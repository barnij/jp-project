ocamllex lexer.mll
ocamlc -c syntax.mli
ocamlc -c syntax.ml
menhir --dump --explain --infer parser.mly
make