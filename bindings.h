/**
 * Runtime environment for subsetphp
 */

#ifndef SUBSETPHP_BINDINGS_H
#define SUBSETPHP_BINDINGS_H

#include "caml/mlvalues.h"

/* Accessing the zend_string* part of an OCaml custom block */
#define Zend_string_val(v) (*((zend_string **) Data_custom_val(v)))

value subsetphp_string_alloc(size_t len, int persistent);
value subsetphp_string_init(const char *str, size_t len, int persistent);
value subsetphp_concat_function(value v1, value v2);
void subsetphp_gc_init();

uintnat caml_max_stack_size;            /* also used in gc_ctrl.c */

/* Declaration of variables used in the asm code */
char * caml_top_of_stack;
extern char * caml_bottom_of_stack;
extern uintnat caml_last_return_address;
extern value * caml_gc_regs;
extern char * caml_exception_pointer;
extern value caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

#endif
