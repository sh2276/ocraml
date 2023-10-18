build:
	dune build

clean:
	dune clean

test:
	OCAMLRUNPARAM=b dune exec test/main.exe