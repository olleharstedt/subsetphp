/**
 * Trying to implement simple mark-sweep from this blog:
 * http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/
 */

#include <stdio.h>
#include <stdint.h>
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

// Set to 1 to enable output
//#define DEBUG 1

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
  //int test_value;
} malloc_list;

static malloc_list* mallocs;
static int mallocs_length;  // Length of mallocs list
static malloc_list* last_malloc;  // Head

static void mark(void);
static void sweep(void);
static void print_mallocs_length(void);

struct structs_gc_info_ {
  int32_t i;
  int32_t j;
  int32_t k;
};
extern struct structs_gc_info_** structs_gc_info[];

/**
 * 22:51:15 - eddyb: so you said the definition is:
 * 22:51:16 - eddyb: @structs_gc_info = global [2 x i8**] [i8** @structs_gc_info_ptr_Something, i8** @structs_gc_info_ptr_Point]
 * 22:51:52 - eddyb: that would translate directly to extern char** structs_gc_info[2]; AFAIK
 */

void llvm_gc_initialize(unsigned int heapsize) {
  mallocs_length = 0;
  mallocs = NULL;
  last_malloc = NULL;

  // Test structs_gc_info
  //char** s = structs_gc_info;
  //char** s1 = structs_gc_info + 16;
  struct structs_gc_info_* stru = *(structs_gc_info[2]);
  int32_t i = stru->i;
  //int32_t j = stru->j;
  printf("i = %d\n", i);
  //printf("j = %d\n", j);
  //printf("i2 = %d\n", i2);
  //printf("j = %d\n", j);

  printf("sizeof(char) = %lu\n", sizeof(void*)); 

  /*
    printf("Initializing heap: %d \n", heapsize);

    f_heap = (byte*) malloc(heapsize);
    memset(f_heap, 0, heapsize);
    f_limit = f_heap + heapsize - 1;

    t_alloc = f_heap;
  */
}

void llvm_gc_shutdown() {
#ifdef DEBUG
    printf("llvm_gc_shutdown()\n");
#endif
}

void llvm_gc_collect() {
#ifdef DEBUG
    printf("llvm_gc_collect()\n");
#endif

    mark();
    sweep();
}

static int nr_of_allocs = 0;
static int nr_of_frees = 0;

/**
 * Allocate memory
 * Uses a memory block to keep track
 * of all mallocs in the system.
 *
 * @param int size
 * @return
 */
void* llvm_gc_allocate(unsigned int size) {

    nr_of_allocs++;

#ifdef DEBUG
    printf("nr_of_allocs = %d\n", nr_of_allocs);
#endif

    if (nr_of_allocs % 100 == 5) {
      llvm_gc_collect();
    }

    void* res = malloc(size);
    if (res == NULL) {
      printf("Out of memory\n");
      exit(1);
    }

    // Set type info
    zend_string* str = (zend_string*) res;
    str->gc.u.type_info = 0;
    str->gc.refcount = 0;  // Used in mark phase

    // Add malloc to list of all mallocs (used by sweep phase)
    malloc_list* malloc_header = malloc(sizeof (malloc_list));

    if (malloc_header == NULL) {
      printf("Out of memory\n");
      exit(1);
    }

    malloc_header->value = res;
    malloc_header->next = NULL;
    //malloc_header->test_value = nr_of_allocs;
    mallocs_length++;

    if (last_malloc != NULL) {
#ifdef DEBUG
      //printf("last_malloc->test_value = %d\n", last_malloc->test_value);
#endif
      last_malloc->next = malloc_header;
      last_malloc = malloc_header;
    }
    else if (mallocs == NULL) {
      last_malloc = malloc_header;
      mallocs = malloc_header;
    }
    else {
      assert(false);
    }

#ifdef DEBUG
    print_mallocs_length();
#endif

    /*
    if (mallocs_length == 1) {
      mallocs = malloc_header;
    }
    else if (mallocs_length > 1) {

      if (last_malloc != NULL) {
        last_malloc->next = malloc_header;
      }

    }
    else {
      assert(false);
    }
    */


    return res;
}

//void mark(zend_string* object) {
    //object->gc.refcount = 1;
//}

/**
 * Mark all malloc blocks that are not
 * in gcroot list.
 */
static void mark() {

#ifdef DEBUG
    printf("mark begin\n");
#endif

  struct StackEntry *entry = llvm_gc_root_chain;
  int j = 0;

  while (entry) {

    //prints((zend_string*) entry->Roots[0]);
    int num_roots = entry->Map->NumRoots;
#ifdef DEBUG
    printf("num_roots = %d\n", num_roots);
#endif
    for (int i = 0; i < num_roots; i++) {
        // TODO: What if root is not string? Like struct, array.
        zend_string* root = (zend_string *) entry->Roots[i];

        if (root) {

#ifdef DEBUG
          printf("  root = %p\n", root);
          printf("  sizeof root = %lu\n", sizeof(root));
          printf("  type_info = %d\n", root->gc.u.type_info);
          printf("  refcount = %d\n", root->gc.refcount);
#endif
          if (root->gc.u.type_info == 262 || root->gc.u.type_info == 0) {  // 262 = string?
#ifdef DEBUG
            printf("  set refcount to 1\n");
#endif
            root->gc.refcount = 1;

            // TODO: Traverase children of struct/object/array
            // Use meta-information?
            // Get struct gc info from type_info
            // struct structs_gc_info type_info = structs_gc_info[gc.u.type_info]
            // for each pointer offset in type_info
            // sweep_child(root[pointer_offset]);
          }
        }
    }

    j++;
    entry = entry->Next;
  }

#ifdef DEBUG
  printf("gcroots: %d\n", j);
  printf("mark end\n");
#endif

}

/**
 * Sweep and free all malloc
 * blocks that has refcount == 0
 *
 * @return void
 */
static void sweep()
{
  malloc_list* m;

#ifdef DEBUG
  printf("sweep begin\n");
#endif

  m = mallocs;
  malloc_list* prev = NULL;
  malloc_list* tmp = m;
  int i = 0;
  while (m) {
    i++;
    // TODO: Not only string
    zend_string* str = (zend_string*) m->value;

#ifdef DEBUG
    printf("  refcount = %d\n", str->gc.refcount);
    printf("  m = %p\n", m);
    printf("  m->next = %p\n", m->next);
#endif

    if (str->gc.refcount == 0) {
      
      if (m->value) {

        // Set next pointer of previous block
        if (prev && prev->next) {
          prev->next = m->next;
        }

        tmp = m;
        m = tmp->next;
        tmp->next = NULL;
        free(tmp->value);

        // If this is the first block
        if (tmp == mallocs) {
          mallocs = m;
        }

        // If this is the last block
        if (tmp == last_malloc) {
          last_malloc = prev;
        }
        
        free(tmp);
        nr_of_frees++;
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
      prev = m;
      m = m->next;
    }

  }

#ifdef DEBUG
  printf("mallocs length = %d\n", i);
  printf("sweep end\n");
#endif
}

static void print_mallocs_length(void) {
#ifdef DEBUG
  malloc_list* m;
  int l = 0;
  m = mallocs;
  while(m != NULL) {
    m = m->next;
    l++;
  }
  printf("mallocs length = %d\n", l);
#endif
}
