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

#include "bindings.h"

static int nr_of_free = 0;

static void subsetphp_finalize_string(value v) {

  CAMLparam1(v);
  
  zend_string *str = Zend_string_val(v);

  nr_of_free++;

  free(str);

  CAMLreturn0;
}

static struct custom_operations subsetphp_zend_string = {
  "subsetphp.zend_string",
  subsetphp_finalize_string,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

extern double printd(double x) {
  printf("%f\n", x);
  return 0;
}

extern double prints(value v) {
  zend_string *str = Zend_string_val(v);
  printf("%s\n", str->val);
  return 0;
}

/* String functions */

/* Code below from OCaml C API documentation */

/* Encapsulation of zend_string
   as OCaml custom blocks. */

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

  // Cast from value to zend_string* ?
	//zend_string *str = (zend_string*) caml_alloc(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1), 0);
	zend_string *str = malloc(ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));

  //printf("sizeof(str1) = %d\n", sizeof(*str));
  //printf("ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE etc) = %d\n", ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));
	//zend_string *str = (zend_string *)caml_alloc((len + 1), 0);

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

  value v = caml_alloc_custom(&subsetphp_zend_string, sizeof(zend_string *), 1, 1000);
  Zend_string_val(v) = str;

  return v;
}

/**
 * Returns Val_unit if realloc happened
 * Otherwise the pointer to the new value
 */
extern value subsetphp_string_realloc(value v1, zend_string *str, size_t len) {

  CAMLparam1(v1);

  //printf("len = %d\n", len);
	zend_string *new_str = realloc(str, ZEND_MM_ALIGNED_SIZE(_STR_HEADER_SIZE + len + 1));

  if (new_str == str) {
    // realloc happened, pointer not moved, using same value
    return Val_unit;
  }
  else {
    // no realloc, pointer moved
    // str need to be freed
    //free(str);
    //v = caml_alloc_custom(&subsetphp_zend_string, sizeof(zend_string *), 1, 16);
    Zend_string_val(v1) = new_str;

    GC_REFCOUNT(new_str) = 1;
    /* optimized single assignment */
    GC_TYPE_INFO(new_str) = IS_STRING | ((1 ? IS_STR_PERSISTENT : 0) << 8);
    new_str->h = 0;
    new_str->len = len;

    return v1;
  }

}

/**
 * All alloc functions must be changed to play nicely with
 * the OCaml GC
 *
 * @return value
 */ 
extern value subsetphp_string_init(const char *str, size_t len, int persistent)
{
	value v = subsetphp_string_alloc(len, persistent);

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
extern value subsetphp_concat_function(value v1, value v2) 
{

  CAMLparam2(v1, v2);

  zend_string *str1 = Zend_string_val(v1);
  zend_string *str2 = Zend_string_val(v2);
  size_t str1_len = str1->len;
  size_t str2_len = str2->len;
  size_t result_len = str1_len + str2_len;
  //printf("str1->len = %d, ", str1_len);
  //printf("str2->len = %d\n", str2_len);

  CAMLlocal1(result);
	result = subsetphp_string_alloc(result_len, 1);
  //result = subsetphp_string_init("", result_len, 1);
  zend_string *zend_result = Zend_string_val(result);

  // TODO: What to do here?
  if (str1_len > SIZE_MAX - str2_len) {
    zend_error_noreturn(E_ERROR, "String size overflow");
  }

  //printf("str1->len = %d, ", str1->len);
  //printf("strlen(str1->val) = %d\n", strlen(str1->val));

  memcpy(zend_result->val, str1->val, str1_len);
  memcpy(zend_result->val + str1_len, str2->val, str2_len);
  zend_result->len = result_len;
  zend_result->val[result_len] = '\0';

  //printf("result = %s\n", zend_result->val);

  CAMLreturn(result);
}

// As above but with realloc
extern value subsetphp_concat_function2(value v1, value v2) 
{

  CAMLparam2(v1, v2);

  zend_string *str1 = Zend_string_val(v1);
  zend_string *str2 = Zend_string_val(v2);
  size_t str1_len = str1->len;
  size_t str2_len = str2->len;
  size_t result_len = str1_len + str2_len;
  //printf("str1->len = %d, ", str1_len);
  //printf("str2->len = %d\n", str2_len);

  // TODO: What to do here?
  if (str1_len > SIZE_MAX - str2_len) {
    zend_error_noreturn(E_ERROR, "String size overflow");
  }

  CAMLlocal1(result);
	result = subsetphp_string_realloc(v1, str1, result_len);
  zend_string *zend_result;

  if (result == Val_unit) {
    // realloc, use same value again
    zend_result = Zend_string_val(v1);
    memcpy(zend_result->val + str1_len, str2->val, str2_len);
    zend_result->len = result_len;
    zend_result->val[result_len] = '\0';

    //printf("result = %s\n", zend_result->val);

    CAMLreturn(v1);
  }
  else {
    // no realloc
    //printf("new pointer, no realloc\n");
    zend_result = Zend_string_val(result);
    memcpy(zend_result->val, str1->val, str1_len);

    memcpy(zend_result->val + str1_len, str2->val, str2_len);
    zend_result->len = result_len;
    zend_result->val[result_len] = '\0';

    //printf("result = %s\n", zend_result->val);

    CAMLreturn(result);
  }

}

/**
 * Init the OCaml GC
 */
extern void subsetphp_gc_init() {
  char tos;
  caml_top_of_stack = &tos;
  caml_parse_ocamlrunparam();

  //#define Minor_heap_def 262144
  caml_init_minor_heap_wsz = 262144 / 16;

  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free);


  //caml_init_stack (caml_init_max_stack_wsz);
}

/**
 * Benchmark string concat with realloc
 */
/*
int main(void) {

  CAMLparam0();

  subsetphp_gc_init();

  CAMLlocal1(val1);
  CAMLlocal1(val2);
  CAMLlocal1(val_tmp);

  val1 = subsetphp_string_init("asd", 3, 1);
  val2 = subsetphp_string_init("qwe", 3, 1);

  for (int i = 0; i < 100000; i++) {
    val_tmp = subsetphp_concat_function2(val1, val2);
    if (val_tmp == val1) {
      //printf("same, ");
    }
    else {
      //printf("not same, ");
    }
    val1 = val_tmp;
  }

  //zend_string *str = Zend_string_val(val1);
  //printf("val1 = %s\n", str->val);

  printf("nr_of_free = %d\n", nr_of_free);
  printf("end\n");

  CAMLreturn(0);
}
*/

/**
 * Benchmark of OCaml str
 *
 * This is death, since caml_strconcat does strlen(s)
 */
/*
int main(void) {

  CAMLparam0();

  subsetphp_gc_init();

  CAMLlocal1(val1);
  CAMLlocal1(val2);

  val1 = caml_alloc_sprintf("asd");
  val2 = caml_alloc_sprintf("qwe");

  //val1 = caml_alloc_string(3);
  //val2 = caml_alloc_string(3);

  for (int i = 0; i < 100000; i++) {
    char* result = caml_strconcat(2, String_val(val1), String_val(val2));
    val1 = caml_alloc_sprintf(result);
  }

  printf("val1 = %s\n", String_val(val1));

  CAMLreturn(0);
}
*/
