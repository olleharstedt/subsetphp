/**
 * Trying to implement simple mark-sweep from this blog:
 * http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "types.h"
#include "alloc.h"
#include "llvm_gc_support.h"
#include "class.h"
#include "../php-src/Zend/zend_types.h"

static byte *f_heap, *f_limit;
static byte *t_alloc;

void markAll(void);

void llvm_gc_initialize(unsigned int heapsz) {
  /*
    printf("Initializing heap: %d \n", heapsz);
    
    f_heap = (byte*) malloc(heapsz);
    memset(f_heap, 0, heapsz);
    f_limit = f_heap + heapsz - 1;

    t_alloc = f_heap;
  */
}

void llvm_gc_shutdown() {
    printf("| gc_shutdown()\n");
    printf("+-------------------------------\n");
}

void llvm_gc_collect() {
    printf("+-------------------------------\n");
    printf("| gc_collect()\n");
    printf("+-------------------------------\n");

    struct StackEntry   *entry = llvm_gc_root_chain;

    while (entry) {
    }
}

static int nr_of_allocs = 0;

void* llvm_gc_allocate(unsigned int sz) {
    nr_of_allocs++;

    if (nr_of_allocs > 100) {
      exit(0);
    }

    printf("gc_alloc(%d)\n", sz);

    byte *res = malloc(sz);

    markAll();

    /*
    if (t_alloc + sz > f_limit)
    {
        // Need to collect
        printf(" - not enough free heap space, forcing a collection ...\n");
        llvm_gc_collect();

        if (t_alloc + sz > f_limit)
        {
            printf("Fatal: not enough heap space after collection. Available space is %ld bytes, need %d bytes\n", f_limit - t_alloc + 1, sz);
            exit(-1);
        }

        res = t_alloc;
        printf("... new object at 0x%08x of size %d\n", (unsigned int)res, sz);
    }
    else
    {
        // Enough space available, simply increment
        res = t_alloc;
        t_alloc += sz;
        printf(" - new object at 0x%08x, heap size now %ld bytes\n", (unsigned int)res, t_alloc - f_heap);
    }
    */

    return res;
}

void mark(zend_string* object) {
    object->gc.refcount = 1;
}

void markAll() {
  struct StackEntry *entry = llvm_gc_root_chain;
  int j = 0;

  while (entry) {

    int num_roots = entry->Map->NumRoots;
    printf("num_roots = %d\n", num_roots);
    for (int i = 0; i < num_roots; i++) {
        zend_string* root = (zend_string *)entry->Roots[i];
    }

    //mark(vm->stack[i]);
    j++;
    entry = entry->Next;
  }

  printf("entries: %d\n", j);
}
