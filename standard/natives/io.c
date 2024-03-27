#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

Value print(int arg_n, Module* mod, Value* args) {
  if (arg_n < 1) THROW("Print expects at least 1 argument");
  Value v = args[0];

  switch (v.type) {
    case VALUE_INT:
      printf("%lld", v.int_value);
      break;
    case VALUE_FLOAT:
      printf("%f", v.float_value);
      break;
    case VALUE_STRING:
      printf("%s", v.string_value);
      break;
    default:
      THROW("Print expects integer, float, or string arguments");
  }

  return MAKE_INTEGER(0);
}

Value println(int arg_n, Module* mod, Value* args) {
  print(arg_n, mod, args);
  printf("\n");
  return MAKE_INTEGER(0);
}