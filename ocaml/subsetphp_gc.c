/**
 * subsetphp GC from OCaml
 *
 * Compile OCaml, then copy object files to subsetphp/ocaml/byterun
 * Remove .pic files
 *
 * On Ubuntu, check memory usage etc with
 * /usr/bin/time -v ./subsetphp_gc.o  
 *
 * @since 2015-09-08
 * @author Olle Härstedt
 */

#include <stdio.h>

#include "caml/alloc.h"
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

void subsetphp_gc_init() {
  caml_parse_ocamlrunparam();
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free);
  caml_init_stack (caml_init_max_stack_wsz);
}

int main(void) {
  subsetphp_gc_init();

  printf("Running subsetphp GC test\n");

  CAMLlocal1(val);

  for (int i = 0; i < 1000000; i++) {
    val = caml_alloc(1024, 0);
  }
  
  return 0;
}
