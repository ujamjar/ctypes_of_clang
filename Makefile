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
	ocamlbuild -clean

install:
	ocamlfind install coc META ppx_coc.byte \
		_build/src/coc_runtime.mli \
		_build/src/coc_runtime.o \
		_build/src/coc_runtime.cmi \
		_build/src/coc_runtime.cmo \
		_build/src/coc_runtime.cmx 

uninstall:
	ocamlfind remove coc

