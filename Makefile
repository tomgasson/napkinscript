build:
	rm -rf _build
	mkdir _build
	ocamlc -g -bin-annot -I +compiler-libs -o _build/Lang -c Lang.ml
	ocamlc -g -bin-annot -o _build/prog.exe -I _build -I +compiler-libs ocamlcommon.cma Lang.cmo

test: build
	./_build/prog.exe
