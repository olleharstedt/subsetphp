alloc.o: alloc.c
	clang-3.6 -g -c alloc.c

alloc2.o: alloc2.c
	clang-3.6 -g -I ../php-src -I ../php-src/Zend -I ../php-src/TSRM -c alloc2.c

all: alloc.o alloc2.o
