.PHONY: tools clean all lib tests examples

all: lib

tests: foo date ncurses

lib:
	jbuilder build @install

tools: 
	jbuilder build @tools/build-tools

examples:
	jbuilder build @examples

# TODO
#cstdlib:
#	ocamlbuild gencstdlib.byte
#	./gencstdlib.byte > lib/cstdlib.ml

clean:
	rm -fr _build

install:
	jbuilder install

uninstall:
	jbuilder uninstall
