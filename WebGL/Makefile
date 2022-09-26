
all: lines.js polygons.js dashed_lines.js

demo3.byte: demo3.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax \
          -syntax camlp4o -linkpkg -o $@ $^

lines.byte: lines.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax \
          -syntax camlp4o -linkpkg -o $@ $^

polygons.byte: polygons.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax \
          -syntax camlp4o -linkpkg -o $@ $^

dashed_lines.byte: dashed_lines.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax \
          -syntax camlp4o -linkpkg -o $@ $^

binarize: binarize.ml
	ocamlfind ocamlopt -package json-wheel -linkpkg -o binarize binarize.ml

%.js: %.byte
	js_of_ocaml -pretty $^
