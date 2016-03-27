/**
 * Second try with GC to subsetphp
 * Using semi-space copying collector
 *
 * @since 2015-10-11
 */
#include <stdio.h>
#include "php-src/Zend/zend.h"
#include <zend_types.h>
#include <zend_string.h>
#include <math.h>

#include "bindings2.h"
#include "semigc/alloc.h"

static int nr_of_free = 0;

extern double printd(double x) {
  printf("%f\n", x);
  return 0;
}

extern double prints(zend_string* str) {
  printf("%s\n", str->val);
  return 0;
}

extern double sqrt(double x) {
  return sqrt(x);
}

/**
 * Keep track of struct types, so the
 * GC knows what pointers to follow.
 */
typedef struct _structtype {
  int type_nr;
  int nr_of_pointers;  // 
  int* pointer_offsetts;
  struct _structtype* next;
} structtype;
extern structtype* structtypes;

/* String functions */

/**
 * Allocate memory for a string of length len
 *
 * @param size_t len
 * @param int persistent Rest from Zend
 * @return value
 */
extern zend_string* subsetphp_string_alloc(size_t len, int persistent)
{
  //#define pemalloc(size, persistent) ((persistent)?__zend_malloc(size):emalloc(size))
  /*
  zend_always_inline static void * __zend_malloc(size_t len)
  {
    void *tmp = malloc(len);
    if (tmp) {
      return tmp;
    }
    fprintf(stderr, "Out of memory\n");
    exit(1);
  }
  */
  // TODO: How to allocate this memory so that OCaml GC can collect it?
	//zend_string *str = (zend_string *)pemalloc(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1), persistent);

  // Cast from value to zend_string* ?
	//zend_string *str = (zend_string*) caml_alloc(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1), 0);
	zend_string *str = llvm_gc_allocate(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));

  //printf("sizeof(str1) = %d\n", sizeof(*str));
  //printf("ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE etc) = %d\n", ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));
	//zend_string *str = (zend_string *)caml_alloc((len + 1), 0);

	GC_REFCOUNT(str) = 0;
#if 1
	/* optimized single assignment */
	GC_TYPE_INFO(str) = IS_STRING | ((persistent ? IS_STR_PERSISTENT : 0) << 8);
#else
	GC_TYPE(ret) = IS_STRING;
	GC_FLAGS(ret) = (persistent ? IS_STR_PERSISTENT : 0);
	GC_INFO(ret) = 0;
#endif
	str->h = 0;
	str->len = len;

  //value v = caml_alloc_custom(&subsetphp_zend_string, sizeof(zend_string *), 1, 1000);

  return str;
}

/**
 * Returns Val_unit if realloc happened
 * Otherwise the pointer to the new value
 */
/*
extern value subsetphp_string_realloc(value v1, zend_string *str, size_t len) {

  //printf("len = %d\n", len);
	zend_string *new_str = realloc(str, ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));

  if (new_str == str) {
    // realloc happened, pointer not moved, using same value
    return NULL;
  }
  else {
    // no realloc, pointer moved
    // str need to be freed
    //free(str);
    //v = caml_alloc_custom(&subsetphp_zend_string, sizeof(zend_string *), 1, 16);

    GC_REFCOUNT(new_str) = 1;
    GC_TYPE_INFO(new_str) = IS_STRING | ((1 ? IS_STR_PERSISTENT : 0) << 8);
    new_str->h = 0;
    new_str->len = len;

    return v1;
  }

}
*/

/**
 * All alloc functions must be changed to play nicely with
 * the OCaml GC
 *
 * @return value
 */ 
extern zend_string* subsetphp_string_init(const char *str, size_t len, int persistent)
{
	zend_string* ret = subsetphp_string_alloc(len, persistent);

	memcpy(ret->val, str, len);
	ret->val[len] = '\0';
	return ret;
}

/**
 * Contcat two zend string and store in result
 * Original function in zend_operators.c
 *
 * @return int
 */
extern zend_string* subsetphp_concat_function(zend_string* str1, zend_string* str2)
{

  size_t str1_len = str1->len;
  size_t str2_len = str2->len;
  size_t result_len = str1_len + str2_len;
  //printf("str1->len = %d, ", str1_len);
  //printf("str2->len = %d\n", str2_len);

	zend_string *zend_result = subsetphp_string_alloc(result_len, 1);
  //result = subsetphp_string_init("", result_len, 1);

  // TODO: What to do here?
  if (str1_len > SIZE_MAX - str2_len) {
    printf("subsetphp_concat_function: str1_len > SIZE_MAX - str2_len\n");
    exit(1);
    //zend_error_noreturn(E_ERROR, "String size overflow");
  }

  //printf("str1->len = %d, ", str1->len);
  //printf("strlen(str1->val) = %d\n", strlen(str1->val));

  memcpy(zend_result->val, str1->val, str1_len);
  memcpy(zend_result->val + str1_len, str2->val, str2_len);
  zend_result->len = result_len;
  zend_result->val[result_len] = '\0';

  return zend_result;
  //printf("result = %s\n", zend_result->val);

}

/**
 * Init the OCaml GC
 */
extern void subsetphp_gc_init() {
  llvm_gc_initialize(100000);
}

extern void subsetphp_add_new_structtype(zend_string* name, int lenght, int* pointer_offsets) {
}
