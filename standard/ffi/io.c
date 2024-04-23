#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>
#include <stdbool.h>

#include "cons.h"

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
  switch (get_type(v)) {
    case TYPE_MUTABLE: {
        printf("mut ");
        print_helper(GET_MUTABLE(v));
        break;
    }
    case TYPE_INTEGER:
      printf("%d", (int32_t) v);
      break;
    case TYPE_FLOAT:
      printf("%f", GET_FLOAT(v));
      break;
    case TYPE_STRING:
      printf("\"%s\"", GET_STRING(v));
      break;
    case TYPE_LIST: {
      HeapValue* p = GET_PTR(v);
      printf("[");
      for (int i = 0; i < p->length; i++) {
        print_helper(p->as_ptr[i]);
        if (i < p->length - 1) printf(", ");
      }
      printf("]");
      break;
    }
    case TYPE_SPECIAL:
      printf("<special>");
      break;
    
    case TYPE_UNKNOWN: {
      printf("<unknown>");
      break;
    }
  }
}

Value print(int arg_n, Module* mod, Value* args) {
  if (arg_n < 1) THROW("Print expects at least 1 argument");
  Value v = args[0];

  ASSERT(get_type(v) == TYPE_STRING, "Print expects a string argument");

  printf("%s", GET_STRING(v));

  return MAKE_INTEGER(0);
}

Value println(int arg_n, Module* mod, Value* args) {
  if (arg_n < 1) THROW("Print expects at least 1 argument");
  Value v = args[0];
  ASSERT(get_type(v) == TYPE_STRING, "Println expects a string argument");

  printf("%s\n", GET_STRING(v));
  return MAKE_INTEGER(0);
}

Value does_file_exist(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("DoesFileExists expects 1 argument");
  Value file = args[0];
  ASSERT(get_type(file) == TYPE_STRING,
         "DoesFileExists expects a string argument");

  return MAKE_INTEGER(file_exists(GET_STRING(file)));
}

Value get_args(int arg_n, Module* mod, Value* args) {
  return MAKE_LIST(mod->argv, mod->argc);
}

Value print_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("PrintInt expects 1 argument");
  Value v = args[0];
  ASSERT(get_type(v) == TYPE_INTEGER, "PrintInt expects an integer argument");

  printf("%d\n", (int32_t) v);
  return MAKE_INTEGER(0);
}

Value execute_command(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("ExecuteCommand expects 1 argument");
  Value cmd = args[0];

  ASSERT(get_type(cmd) == TYPE_STRING,
         "ExecuteCommand expects a string argument");

  return MAKE_INTEGER(system(GET_STRING(cmd)));
}

Value ffi_get_index(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("GetIndex expects 2 arguments");
  Value ls = args[0];
  Value idx_v = args[1];

  ASSERT(get_type(ls) == TYPE_LIST, "GetIndex expects a list argument");
  ASSERT(get_type(idx_v) == TYPE_INTEGER, "GetIndex expects an integer argument");

  int idx = GET_INT(idx_v);
  HeapValue* l = GET_PTR(ls);

  if (idx < 0 || idx >= l->length) return make_none();

  return make_some(l->as_ptr[idx]);
}

Value input(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("Input expects 1 argument");
  Value prompt = args[0];
  ASSERT(get_type(prompt)== TYPE_STRING, "Input expects a string argument");

  
  char* buffer = malloc(1024);
  printf("%s", GET_STRING(prompt));
  scanf("%s", buffer);

  return MAKE_STRING(buffer, strlen(buffer));
}