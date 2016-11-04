.PHONY: tools clean all lib
all: lib tools 

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

clean:
	ocamlbuild -clean

