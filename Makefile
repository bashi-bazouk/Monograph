
monograph: src/Util.ml src/Util.ml src/OCamlDep.ml src/TransitiveClosure.ml src/Monograph.ml
	ocamlbuild -I src -libs str,unix Monograph.native
	mv Monograph.native monograph

install: monograph
	cp monograph /usr/local/bin/monograph
