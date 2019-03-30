build:
	ocamlc -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cma Napkinscript.ml

build-native:
	ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa Napkinscript.ml

test: build-native
	./node_modules/.bin/jest

debug: build
	./lib/napkinscript.exe -recover file.js

debugi: build
	./lib/napkinscript.exe file.jsi

release:
	ocamlfind ocamlopt -O3 -o ./lib/napkinscript.exe Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg
