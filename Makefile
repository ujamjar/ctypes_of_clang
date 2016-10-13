.PHONY: tools clean all
all: tools

tools: 
	ocamlbuild ctypes_of_clang.cma genenums.byte dump.byte dumpx.byte

clean:
	ocamlbuild -clean
	rm -f csrc/.o
	rm -f testc

###################################################################
# c-example - derepreciated

csrc/parse.o: csrc/parse.c
	gcc -c `llvm-config-3.8 --cflags` csrc/parse.c -o csrc/parse.o

testc: csrc/parse.o csrc/main.c
	gcc -o testc csrc/main.c csrc/parse.o `llvm-config-3.8 --ldflags` -lclang-3.8

