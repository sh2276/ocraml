build:
	dune build
	cp _build/default/bin/gui.exe gui.exe

clean:
	dune clean
	rm -f gui.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

gui:
	OCAMLRUNPARAM=b dune exec bin/gui.exe