.PHONY: tools clean all lib
all: lib tools 

lib:
	ocamlbuild ctypes_of_clang.cma ctypes_of_clang.cmxa

tools: 
	ocamlbuild genenums.byte info.byte extract.byte ppx_coc.byte

date: lib
	ocamlbuild test_date.byte test_date.native

clean:
	ocamlbuild -clean

