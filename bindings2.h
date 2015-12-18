/**
 * Runtime environment for subsetphp
 */

#ifndef SUBSETPHP_BINDINGS_H
#define SUBSETPHP_BINDINGS_H

zend_string* subsetphp_string_alloc(size_t len, int persistent);
zend_string* subsetphp_string_init(const char *str, size_t len, int persistent);
zend_string* subsetphp_concat_function(zend_string* v1, zend_string* v2);
void subsetphp_gc_init();

#endif
