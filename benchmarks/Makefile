all: concat1

INCLUDES=-I ../ -I ../php-src/Zend -I ../php-src -I ../php-src/TSRM -I ../php-src/main -I ../ocaml -I ../ocaml/byterun -I ../ocaml/asmrun 
LINK=../php-src/Zend/*.o ../ocaml/byterun/*.o  ../runtime.o
LIBS=-lm -ldl -lncurses

concat1.ll: concat1.c
	clang-3.6 -S -emit-llvm $(INCLUDES) -o concat1.ll concat1.c -O0

concat1: concat1.c
	clang-3.6 -g $(INCLUDES) $(LINK) -o concat1 concat1.c $(LIBS)

nbody: nbody.php
	../llvm_test nbody.php
	llvm-dis nbody.bc
	llc nbody.bc
	clang-3.6 -g -c -m64 nbody.s
	clang-3.6 -g -o nbody -I ../php-src/Zend ../php-src/Zend/*.o ../ocaml/byterun/*.o ../semigc/alloc2.o ../runtime2.o nbody.o -lm -ldl -lncurses
