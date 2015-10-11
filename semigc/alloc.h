// Collector API
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010

#ifndef ALLOC_H
#define ALLOC_H

// Garbage collector routines
void    llvm_gc_initialize(unsigned int heapsz);
void*   llvm_gc_allocate(unsigned int sz);
void    llvm_gc_collect();
void    llvm_gc_shutdown();

#endif
