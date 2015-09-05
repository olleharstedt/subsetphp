#include <stdio.h>
#include <zend.h>
#include <zend_types.h>
#include <zend_string.h>

extern double printd(double x) {
  printf("%f\n", x);
  return 0;
}

extern double prints(zend_string* str) {
  printf("%s\n", str->val);
  return 0;
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
