
all:
	mkdir -p bin
	dune build src/eba.exe
	cp _build/default/src/eba.exe bin/eba

clean:
	dune clean
