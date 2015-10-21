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
  struct _malloc_list* prev;
  void* value;
} malloc_list;

static malloc_list* mallocs;
static int mallocs_length;  // Length of mallocs list
static malloc_list* last_malloc;  // Head

void markAll(void);

void llvm_gc_initialize(unsigned int heapsize) {
  mallocs_length = 0;
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

    if (nr_of_allocs == 500) {
      markAll();
    }

    void* res = malloc(size);
    if (res == NULL) {
      printf("Out of memory\n");
      exit(1);
    }

    //((zend_string*)(res))->gc.refcount = 0;  // Done in binding (wrong?)

    // Add malloc to list of all mallocs (used by sweep phase)
    malloc_list* m = malloc(sizeof (malloc_list));

    if (m == NULL) {
      printf("Out of memory\n");
      exit(1);
    }

    m->value = res;
    m->next = NULL;
    m->prev = NULL;
    mallocs_length++;
    printf("mallocs_length = %d\n", mallocs_length);

    if (mallocs_length == 1) {
      mallocs = m;
    }
    else if (mallocs_length > 1) {

      // If mallocs_length > 1 then we've had a malloc before
      //assert(last_malloc != NULL);

      //m->prev = last_malloc;
      //assert(m->prev != NULL);
    }
    else {
      assert(false);
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
  malloc_list* prev = NULL;
  malloc_list* tmp = m;
  while (m) {
    zend_string* str = (zend_string*) m->value;
    printf("  refcount = %d\n", str->gc.refcount);
    printf("  m = %p\n", m);
    printf("  m->next = %p\n", m->next);

    if (str->gc.refcount == 0) {
      
      if (m->value) {
        tmp = m;
        free(m->value);
        m = m->next;
        free(tmp);
      }

      // Four cases?
      // Remove if it is the only malloc block
      // Remove if it is the last malloc block
      // Remove if it is the first
      // Remove if it's in the middle

      // Update last_malloc, should point to last entry in list
      // Update mallocs_length
      // Update prev and next pointer of blocks around the deleted block

    }
    else {
      str->gc.refcount = 0;
      m = m->next;
    }

  }

  printf("entries: %d\n", j);
}
