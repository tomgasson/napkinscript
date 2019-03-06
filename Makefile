build:
	ocamlc -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cma Lang.ml

build-native:
	ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa Lang.ml

test: build-native
	./node_modules/.bin/jest

debug: build
	./lib/napkinscript.exe file.js

release:
	ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa Lang.ml
