build:
	ocamlc -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cma unix.cma Lang.ml

build-native:
	ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa unix.cmxa Lang.ml

test: build-native
	./node_modules/.bin/jest

debug: build
	./lib/napkinscript.exe file.js

debugi: build
	./lib/napkinscript.exe file.jsi

release:
	ocamlopt -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa Lang.ml
