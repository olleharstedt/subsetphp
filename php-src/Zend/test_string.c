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

int main(void) {
	zend_startup_constants();
  zend_string *str = zend_string_init("asd", 4, 1);
  printf(str->val);
  printf("\n");
  return 0;
}
