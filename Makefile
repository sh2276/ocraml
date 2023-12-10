.PHONY: test

build:
	dune build
	rm -f gui.exe
	cp _build/default/bin/gui.exe gui.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f gui.exe
	rm -f ./bin/_build/default/gui.ml

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

gui:
	OCAMLRUNPARAM=b dune exec bin/gui.exe

loader:
	OCAMLRUNPARAM=b dune exec lib/loaderexample.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

