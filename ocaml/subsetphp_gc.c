/**
 * subsetphp GC from OCaml
 *
 * @since 2015-09-08
 * @author Olle Härstedt
 */

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
#ifdef NATIVE_CODE
#include "stack.h"
#else
#include "caml/stacks.h"
#endif

void subsetphp_gc_init() {
}
