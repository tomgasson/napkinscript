build:
	ocamlc -bin-annot -o ./lib/napkinscript.exe -custom mach_stubs.c -I +compiler-libs ocamlcommon.cma refmt_main3.cmo Napkinscript.ml

build-native:
	ocamlopt -bin-annot -O3 -o ./lib/napkinscript.exe -I +compiler-libs ocamlcommon.cmxa refmt_main3.cmx mach_stubs.c Napkinscript.ml

build-refmt-native:
	ocamlopt -c -I +compiler-libs ocamlcommon.cmxa refmt_main3.ml

build-refmt-byte:
	ocamlc -c -I +compiler-libs ocamlcommon.cma refmt_main3.ml

test: build-native
	./node_modules/.bin/jest
	./node_modules/.bin/gentype -termination-cmt ./Napkinscript.cmt

termination:
	./node_modules/.bin/gentype -termination-cmt ./Napkinscript.cmt
