build:
	rm -rf _build
	mkdir _build
	ocamlc -g -bin-annot -I +compiler-libs -o _build/Lang -c Lang.ml
	ocamlc -g -bin-annot -o _build/prog.exe -I _build -I +compiler-libs ocamlcommon.cma Lang.cmo

build-native:
	rm -rf _build
	mkdir _build
	ocamlopt -O3 -o _build/native.exe -I _build -I +compiler-libs ocamlcommon.cmxa Lang.ml

test: build
	./_build/prog.exe
