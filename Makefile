build:
	rm -rf _build
	mkdir _build
	ocamlc -g -bin-annot -I +compiler-libs -o _build/Lang -c Lang.ml
	ocamlc -g -bin-annot -o _build/prog.exe -I _build -I +compiler-libs ocamlcommon.cma Lang.cmo

build-native:
	rm -rf _build
	mkdir _build
	esy	ocamlopt -O3 -o _build/native.exe -I _build -I +compiler-libs ocamlcommon.cmxa Lang.ml

bench:
	./_build/native.exe Bench.re

test: build-native
	esy jest
