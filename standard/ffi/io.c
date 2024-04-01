#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

typedef int bool;
#define true 1
#define false 0

bool file_exists(const char* filename) {
  FILE* fp = fopen(filename, "r");
  bool is_exist = false;
  if (fp != NULL) {
    is_exist = true;
    fclose(fp);
  }
  return is_exist;
}

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

  ASSERT(v.type == VALUE_STRING, "Print expects a string argument");

  printf("%s", v.string_value);

  return MAKE_INTEGER(0);
}

Value println(int arg_n, Module* mod, Value* args) {
  if (arg_n < 1) THROW("Print expects at least 1 argument");
  Value v = args[0];
  ASSERT(v.type == VALUE_STRING, "Println expects a string argument");

  printf("%s\n", v.string_value);
  return MAKE_INTEGER(0);
}

Value does_file_exist(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("DoesFileExists expects 1 argument");
  ASSERT(args[0].type == VALUE_STRING,
         "DoesFileExists expects a string argument");

  return MAKE_INTEGER(file_exists(args[0].string_value));
}

Value get_args(int arg_n, Module* mod, Value* args) {
  return MAKE_LIST(mod->args);
}

Value execute_command(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("ExecuteCommand expects 1 argument");
  ASSERT(args[0].type == VALUE_STRING,
         "ExecuteCommand expects a string argument");

  return MAKE_INTEGER(system(args[0].string_value));
}