.PHONY: test

build:
	dune build
	rm -f gui.exe
	cp _build/default/bin/gui.exe gui.exe

clean:
	dune clean
	rm -f gui.exe
	rm -f ./bin/_build/default/gui.ml

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

gui:
	OCAMLRUNPARAM=b dune exec bin/gui.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

