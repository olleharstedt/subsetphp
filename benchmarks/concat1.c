#include <stdio.h>
#include "zend.h"
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

  CAMLreturn(0);
}
