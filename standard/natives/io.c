#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

void print_helper(Value v) {
  switch (v.type) {
    case VALUE_INT:
      printf("%lld", v.int_value);
      break;
    case VALUE_FLOAT:
      printf("%f", v.float_value);
      break;
    case VALUE_STRING:
      printf("\"%s\"", v.string_value);
      break;
    case VALUE_LIST:
      printf("[");
      for (int i = 0; i < v.list_value.length; i++) {
        print_helper(v.list_value.values[i]);
        if (i < v.list_value.length - 1) printf(", ");
      }
      printf("]");
      break;
    case VALUE_ADDRESS:
      printf("<function 0x%x>", v.address_value);
      break;
    case VALUE_NATIVE:
      printf("<native>");
      break;
    case VALUE_SPECIAL:
      printf("<special>");
      break;
  }
}

Value print(int arg_n, Module* mod, Value* args) {
  if (arg_n < 1) THROW("Print expects at least 1 argument");
  Value v = args[0];

  print_helper(v);

  return MAKE_INTEGER(0);
}

Value println(int arg_n, Module* mod, Value* args) {
  print(arg_n, mod, args);
  printf("\n");
  return MAKE_INTEGER(0);
}

Value get_args(int arg_n, Module* mod, Value* args) {
  return MAKE_LIST(mod->args);
}