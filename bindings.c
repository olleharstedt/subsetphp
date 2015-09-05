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
