.PHONY: tools clean all lib tests
all: lib
tests: foo date ncurses

lib:
	ocamlbuild ctypes_of_clang.cma ctypes_of_clang.cmxa ppx_coc.byte 

tools: 
	ocamlbuild genenums.byte info.byte extract.byte 

foo: lib
	ocamlbuild test_foo.byte test_foo.native

date: lib
	ocamlbuild test_date.byte test_date.native

ncurses: lib
	ocamlbuild test_ncurses.byte test_ncurses.native

cstdlib:
	ocamlbuild gencstdlib.byte
	./gencstdlib.byte > lib/cstdlib.ml

clean:
	ocamlbuild -clean

install:
	ocamlfind install coc META ppx_coc.byte \
		_build/src/coc_runtime.mli \
		_build/src/coc_runtime.cmi \
		_build/src/coc_runtime.cmo \
		_build/src/coc_runtime.cmx 

uninstall:
	ocamlfind remove coc

