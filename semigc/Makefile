alloc.o: alloc.c
	clang-3.6 -c alloc.c -O3

alloc2.o: alloc2.c
	clang-3.6 -I ../php-src -I ../php-src/Zend -I ../php-src/TSRM -c alloc2.c -O3

all: alloc.o alloc2.o
