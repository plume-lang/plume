#include <stdio.h>
#include <value.h>

int facto(int n) {
  if (n == 0) return 1;

  return n * facto(n - 1);
}

Value test(int argc, Value *argv) {
  return MAKE_INTEGER(facto(argv[0].int_value));
}