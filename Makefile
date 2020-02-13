
all:
	mkdir -p bin
	dune build src/eba.exe
	cp _build/default/src/eba.exe bin/eba

debug: 
	dune build src/eba.bc
	cp _build/default/src/eba.bc bin/eba.bc

clean:
	dune clean
