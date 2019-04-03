build:
	# ocamlc -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cma Napkinscript.ml
	ocamlfind ocamlc -o ./lib/napkinscript.exe Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg

build-native:
	# ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa Napkinscript.ml
	ocamlfind ocamlopt -O3 -o ./lib/napkinscript.exe Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg

test: build-native
	./node_modules/.bin/jest

debug: build
	./lib/napkinscript.exe -print ml -recover file.js

debugi: build
	./lib/napkinscript.exe -print ml -recover file.jsi

release: build-native
	# ocamlfind ocamlopt -O3 -o ./lib/napkinscript.exe Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg
