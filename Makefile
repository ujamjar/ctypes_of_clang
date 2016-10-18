.PHONY: tools clean all lib
all: lib tools 

lib:
	ocamlbuild ctypes_of_clang.cma

tools: 
	ocamlbuild genenums.byte info.byte extract.byte ppx_coc.byte

clean:
	ocamlbuild -clean
	rm -f csrc/.o
	rm -f testc

