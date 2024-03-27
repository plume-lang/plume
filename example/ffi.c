#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <value.h>

int facto(int n) {
  if (n == 0) return 1;

  return n * facto(n - 1);
}

Value test(int argc, Module *m, Value *argv) {
  return MAKE_INTEGER(facto(argv[0].int_value));
}

Value mul(int argc, Module *m, Value *argv) {
  ASSERT(argc == 2, "mul expects 2 arguments");
  return MAKE_INTEGER(argv[0].int_value * argv[1].int_value);
}

Value sub(int argc, Module *m, Value *argv) {
  ASSERT(argc == 2, "sub expects 2 arguments");
  return MAKE_INTEGER(argv[0].int_value - argv[1].int_value);
}