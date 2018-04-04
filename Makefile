
all:
	mkdir -p bin
	jbuilder build src/eba.exe
	cp _build/default/src/eba.exe bin/eba

clean:
	jbuilder clean
