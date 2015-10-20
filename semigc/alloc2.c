/**
 * Trying to implement simple mark-sweep from this blog:
 * http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/
 */

#include <stdio.h>
#include <stdbool.h>
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

/**
 * Wrapper around Zend types
 * Needed?
 */
struct object
{
  bool marked;

  void* value;

};

typedef struct _malloc_list {
  struct _malloc_list* next;
  void* value;
} malloc_list;

static malloc_list* mallocs;
static malloc_list* last_malloc;

void markAll(void);

void llvm_gc_initialize(unsigned int heapsize) {
  /*
    printf("Initializing heap: %d \n", heapsize);

    f_heap = (byte*) malloc(heapsize);
    memset(f_heap, 0, heapsize);
    f_limit = f_heap + heapsize - 1;

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

void* llvm_gc_allocate(unsigned int size) {
    nr_of_allocs++;

    if (nr_of_allocs > 1000) {
      exit(0);
    }

    printf("gc_alloc(%d)\n", size);

    //struct object* wrapper = malloc(sizeof (struct object));
    //wrapper->marked = 0;
    //wrapper->value = res;

    markAll();

    void* res = malloc(size);
    //((zend_string*)(res))->gc.refcount = 0;  // Done in binding (wrong?)

    // Add malloc to list of all mallocs (used by sweep phase)
    malloc_list* m = malloc(sizeof (malloc_list));
    m->value = res;

    if (mallocs) {
      last_malloc->next = m;
    }
    else {
      mallocs = m;
    }
    last_malloc = m;

    return res;
}

void mark(zend_string* object) {
    object->gc.refcount = 1;
}

void markAll() {
  struct StackEntry *entry = llvm_gc_root_chain;
  int j = 0;

  while (entry) {

    //prints((zend_string*) entry->Roots[0]);
    int num_roots = entry->Map->NumRoots;
    printf("num_roots = %d\n", num_roots);
    for (int i = 0; i < num_roots; i++) {
        zend_string* root = (zend_string *)entry->Roots[i];
        printf("%p\n", root);
        if (root) {
          printf("  %p\n", root->val);
          printf("  %s\n", root->val);
          printf("  refcount = %d\n", root->gc.refcount);
          root->gc.refcount = 1;
        }
    }

    j++;
    entry = entry->Next;
  }

  // Free all blocks that were not found while scanning roots
  printf("mallocs:\n");
  malloc_list* m = mallocs;
  while (m) {
    zend_string* str = (zend_string*) m->value;
    printf("  %d\n", str->gc.refcount);

    if (str->gc.refcount == 0) {
      if (m == mallocs) {
        mallocs = m->next;
        free(m->value);
        free(m);
      }
      else {
        free(m->value);
        free(m);
      }
    }

    m = m->next;
  }

  // Reset mark stuff
  m = mallocs;
  while (m) {
    zend_string* str = (zend_string*) m->value;
    str->gc.refcount = 0;
    prev = m;
    m = m->next;
  }
  last_malloc = prev;

  printf("entries: %d\n", j);
}
