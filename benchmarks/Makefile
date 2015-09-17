all: concat1

INCLUDES=-I ../ -I ../php-src/Zend -I ../php-src -I ../php-src/TSRM -I ../php-src/main -I ../ocaml -I ../ocaml/byterun -I ../ocaml/asmrun 
LINK=../php-src/Zend/*.o ../ocaml/byterun/*.o  ../runtime.o
LIBS=-lm -ldl -lncurses

concat1: concat1.c
	clang-3.6 -g $(INCLUDES) $(LINK) -o concat1 concat1.c $(LIBS)
