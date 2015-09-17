#include <bindings.h>

/**
 * Test OCaml GC
 *
 * Benchmark: Slow, 2.5 sec vs HHVM 1.5 sec
 * 2015-09-16
 */
int main(void) {

  CAMLparam0();

  subsetphp_gc_init();

  CAMLlocal1(val1);
  CAMLlocal1(val2);

  val1 = subsetphp_string_init("asd", 3, 1);
  val2 = subsetphp_string_init("qwe", 3, 1);

  for (int i = 0; i < 100000; i++) {
    val1 = subsetphp_concat_function(val1, val2);
  }

  //zend_string *str = Zend_string_val(val1);
  //printf("val1 = %s\n", str->val);

  printf("nr_of_free = %d\n", nr_of_free);
  printf("end\n");

  CAMLreturn(0);
}
