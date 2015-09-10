#include <stdio.h>
#include <zend.h>
#include <zend_types.h>
#include <zend_string.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/compact.h"
#include "caml/custom.h"
#include "caml/finalise.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/sys.h"
#include "caml/startup_aux.h"
#ifdef NATIVE_CODE
#include "stack.h"
#else
#include "caml/stacks.h"
#endif

/* Declaration of variables used in the asm code */
extern char * caml_top_of_stack;
extern char * caml_bottom_of_stack;
extern uintnat caml_last_return_address;
extern value * caml_gc_regs;
extern char * caml_exception_pointer;
extern value caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

extern double printd(double x) {
  printf("%f\n", x);
  return 0;
}

extern double prints(zend_string* str) {
  printf("%s\n", str->val);
  return 0;
}

/* String functions */

/* Code below from OCaml C API documentation */

/* Encapsulation of zend_string
   as OCaml custom blocks. */

static struct custom_operations subsetphp_zend_string = {
  "subsetphp.zend_string",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Accessing the zend_string* part of an OCaml custom block */
#define Zend_string_val(v) (*((zend_string **) Data_custom_val(v)))

/**
 * Allocate memory for a string of length len
 *
 * @param size_t len
 * @param int persistent Rest from Zend
 * @return value
 */
extern value subsetphp_string_alloc(size_t len, int persistent)
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
	zend_string *str = (zend_string *)caml_alloc(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1), 0);

	GC_REFCOUNT(str) = 1;
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

  value v = caml_alloc_custom(&subsetphp_zend_string, sizeof(zend_string *), 0, 1);
  Zend_string_val(v) = str;

  return v;
}

/**
 * All alloc functions must be changed to play nicely with
 * the OCaml GC
 *
 * @return value
 */ 
extern value subsetphp_string_init(const char *str, size_t len, int persistent)
{
	value *v = subsetphp_string_alloc(len, persistent);

  zend_string *ret = Zend_string_val(v);

	memcpy(ret->val, str, len);
	ret->val[len] = '\0';
	return v;
}

/**
 * Contcat two zend string and store in result
 * Original function in zend_operators.c
 *
 * @return int
 */
extern zend_string* subsetphp_concat_function(zend_string *str1, zend_string *str2) 
{

  size_t str1_len = str1->len;
  size_t str2_len = str2->len;
  size_t result_len = str1_len + str2_len;

  zend_string *result = zend_string_init("", result_len, 1);

  if (str1_len > SIZE_MAX - str2_len) {
    zend_error_noreturn(E_ERROR, "String size overflow");
  }

  memcpy(result->val, str1->val, str1_len);
  memcpy(result->val + str1_len, str2->val, str2_len);
  result->len = result_len;
  result->val[result_len] = '\0';

	return result;
}

/**
 * Init the OCaml GC
 */
extern void subsetphp_gc_init() {
  caml_parse_ocamlrunparam();
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free);
  //caml_init_stack (caml_init_max_stack_wsz);
}

