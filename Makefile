build:
	ocamlc -bin-annot -o ./lib/napkinscript.exe -custom mach_stubs.c -I +compiler-libs ocamlcommon.cma Napkinscript.ml
	# ocamlfind ocamlc -o ./lib/napkinscript.exe -custom mach_stubs.c Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg

build-native:
	ocamlopt -bin-annot -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa mach_stubs.c Napkinscript.ml
	# ocamlfind ocamlopt -O3 -o ./lib/napkinscript.exe refmt_main3.cmx mach_stubs.c Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg
	# ocamlfind ocamlopt -O3 -o ./lib/napkinscript.exe mach_stubs.c Napkinscript.ml -package ocaml-migrate-parsetree -linkpkg

build-refmt:
	ocamlopt -c -I +compiler-libs ocamlcommon.cmxa refmt_main3.ml

test: build-native
	./node_modules/.bin/jest
