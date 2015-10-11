#!/bin/sh -x

# We need to build these two .c files with llvm-gcc as we
# use __attribute__((gcroot)), which only llvm-gcc knows about
# (and not vanilla gcc).
#
# We also can't turn these .c files into object files directly, as the
# shadow stack code doesn't get included that way. Going through
# llc is needed.
llvm-gcc-4.8 -g -W -Wall -fno-strict-aliasing -c -emit-llvm main.c -o main.bc -std=c99
#llvm-gcc-4.8 -g -W -Wall -fno-strict-aliasing -c -emit-llvm class.c -o class.bc -std=c99
#llc -o main.s main.bc
#llc -o class.s class.bc
#as -o main.o main.s
#as -o class.o class.s

# Make final executable
#gcc -g -W -Wall -o m -fno-strict-aliasing -std=c99 alloc.c class.o main.o
