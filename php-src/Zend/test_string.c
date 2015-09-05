/*
 * Test Zend strings
 *
 * @author Olle Harstedt
 * @since 2015-09-02
 */

#include <zend.h>
#include <zend_string.h>

/*
struct _zend_string {
	zend_refcounted   gc;
	zend_ulong        h;
	size_t            len;
	char              val[1];
};
*/

int subsetphp_concat_function(zend_string *result, zend_string *str1, zend_string *str2) /* {{{ */
{

  // Disallow this for now (pointer stuff)
  assert(result != str1);

  size_t str1_len = str1->len;
  size_t str2_len = str2->len;
  size_t result_len = str1_len + str2_len;

  if (str1_len > SIZE_MAX - str2_len) {
    zend_error_noreturn(E_ERROR, "String size overflow");
  }

  //if (result == str1) { // && Z_REFCOUNTED_P(result)) {
    /* special case, perform operations on result */
    //result = zend_string_realloc(str1, result_len, 1);
  //} else {
    memcpy(result->val, str1->val, str1_len);
  //}

  memcpy(result->val + str1_len, str2->val, str2_len);
  result->len = result_len;
  result->val[result_len] = '\0';
  printf("subsetphp_concat_function: %s\n", result->val);

	return SUCCESS;
}
/* }}} */

void test(zend_string *str) {
  str->len = 10;
}

int main(void) {

  // Alloc init

	zend_startup_constants();
  zend_interned_strings_init();

  zend_string *str = zend_string_init("asd", 3, 1);
  int is_int = IS_INTERNED(str);
  printf("%d\n", is_int);
  zend_new_interned_string(str);  // Do this for string literals
  is_int = IS_INTERNED(str);
  printf("%d\n", is_int);
  printf("%s\n", str->val);

  // Concat two string variables
  zend_string *str2 = zend_string_init("qwe", 3, 1);
  zend_string *result = zend_string_init("", 0, 1);
  subsetphp_concat_function(result, str, str2);
  printf("%s\n", result->val);
  printf("%i\n", result->len);

  return 0;
}
