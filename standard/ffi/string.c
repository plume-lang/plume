#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

#include "cons.h"

Value add_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT_FMT(args[0].type == VALUE_STRING && args[1].type == VALUE_STRING,
             "Add expects string arguments, received %d and %d", args[0].type,
             args[1].type);

  char* new_str =
      malloc(strlen(args[0].string_value) + strlen(args[1].string_value));
  strcpy(new_str, args[0].string_value);
  strcat(new_str, args[1].string_value);

  return MAKE_STRING(new_str);
}

Value mul_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(args[0].type == VALUE_STRING && args[1].type == VALUE_INT,
         "Mul expects string and integer arguments");

  char* new_str = malloc(strlen(args[0].string_value) * args[1].int_value + 1);
  new_str[0] = '\0';

  for (int i = 0; i < args[1].int_value; i++) {
    strcat(new_str, args[0].string_value);
  }

  return MAKE_STRING(new_str);
}

Value to_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("To_string expects 1 argument");
  switch (args[0].type) {
    case VALUE_MUTABLE: {
      Value v = *args[0].mutable_value;
      char* str = to_string(1, mod, &v).string_value;
      size_t sz = strlen(str) + 5;
      char* new_str = malloc(sz * sizeof(char));
      strcpy(new_str, "mut ");
      strcpy(new_str + 4, str);
      return MAKE_STRING(new_str);
    }
    case VALUE_INT: {
      size_t sz = snprintf(NULL, 0, "%lld", args[0].int_value);
      char* new_str = malloc((sz + 1) * sizeof(char));
      sprintf(new_str, "%lld", args[0].int_value);
      return MAKE_STRING(new_str);
    }
    case VALUE_FLOAT: {
      size_t sz = snprintf(NULL, 0, "%f", args[0].float_value);
      char* new_str = malloc((sz + 1) * sizeof(char));
      sprintf(new_str, "%f", args[0].float_value);
      return MAKE_STRING(new_str);
    }
    case VALUE_STRING: {
      char* str = args[0].string_value;
      size_t sz = strlen(str);
      char* new_str = malloc(sz + 3);
      new_str[0] = '"';
      strcpy(new_str + 1, str);
      new_str[sz + 1] = '"';
      return MAKE_STRING(new_str);
    }
    case VALUE_LIST: {
      size_t sz = 2;
      for (int i = 0; i < args[0].list_value.length; i++) {
        sz += strlen(
            to_string(1, mod, &args[0].list_value.values[i]).string_value);
      }

      char* new_str = malloc(sz * sizeof(char));
      new_str[0] = '[';
      new_str[1] = '\0';

      for (int i = 0; i < args[0].list_value.length; i++) {
        strcat(new_str,
               to_string(1, mod, &args[0].list_value.values[i]).string_value);
        if (i < args[0].list_value.length - 1) strcat(new_str, ", ");
      }

      strcat(new_str, "]");

      return MAKE_STRING(new_str);
    }
    case VALUE_ADDRESS: {
      size_t sz = snprintf(NULL, 0, "<function 0x%x>", args[0].address_value);
      char* new_str = malloc((sz + 1) * sizeof(char));
      sprintf(new_str, "<function 0x%x>", args[0].address_value);
      return MAKE_STRING(new_str);
    }
    case VALUE_NATIVE: {
      return MAKE_STRING("<native>");
    }
    case VALUE_SPECIAL: {
      return MAKE_STRING("<special>");
    }
  }

  return MAKE_STRING("unknown");
}

Value string_length(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("String_length expects 1 argument");
  ASSERT(args[0].type == VALUE_STRING,
         "String_length expects a string argument");

  return MAKE_INTEGER(strlen(args[0].string_value));
}

Value eq_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(args[0].type == VALUE_STRING && args[1].type == VALUE_STRING,
         "Eq expects string arguments");

  return MAKE_INTEGER(strcmp(args[0].string_value, args[1].string_value) == 0);
}

Value list_append(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_append expects 2 arguments");
  ASSERT(args[0].type == VALUE_LIST,
         "List_append expects a list as the first argument");

  Value list = args[0];
  Value value = args[1];

  ValueList new_list;
  new_list.length = list.list_value.length + 1;
  new_list.values = malloc(new_list.length * sizeof(Value));

  for (int i = 0; i < list.list_value.length; i++) {
    new_list.values[i] = list.list_value.values[i];
  }

  new_list.values[list.list_value.length] = value;

  return MAKE_LIST(new_list);
}

Value list_prepend(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_prepend expects 2 arguments");
  ASSERT(args[0].type == VALUE_LIST,
         "List_prepend expects a list as the first argument");

  Value list = args[0];
  Value value = args[1];

  ValueList new_list;
  new_list.length = list.list_value.length + 1;
  new_list.values = malloc(new_list.length * sizeof(Value));

  new_list.values[0] = value;
  for (int i = 0; i < list.list_value.length; i++) {
    new_list.values[i + 1] = list.list_value.values[i];
  }

  return MAKE_LIST(new_list);
}

Value list_concat(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_concat expects 2 arguments");
  ASSERT(args[0].type == VALUE_LIST && args[1].type == VALUE_LIST,
         "List_concat expects list arguments");

  ValueList new_list;
  new_list.length = args[0].list_value.length + args[1].list_value.length;
  new_list.values = malloc(new_list.length * sizeof(Value));

  size_t sz = args[0].list_value.length;

  for (int i = 0; i < args[0].list_value.length; i++) {
    new_list.values[i] = args[0].list_value.values[i];
  }

  for (int i = 0; i < args[1].list_value.length; i++) {
    new_list.values[i + sz] = args[1].list_value.values[i];
  }

  return MAKE_LIST(new_list);
}

Value get_index_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("GetIndex expects 2 arguments");
  ASSERT(args[0].type == VALUE_STRING && args[1].type == VALUE_INT,
         "GetIndex expects string and integer arguments");

  int idx = args[1].int_value;
  char* str = args[0].string_value;

  if (idx < 0 || idx >= strlen(str)) return make_none();

  return make_some(MAKE_CHAR(str[idx]));
}

Value char_to_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("CharToString expects 1 argument");
  ASSERT(args[0].type == VALUE_STRING,
         "CharToString expects a string argument");
  return MAKE_STRING(args[0].string_value);
}

Value eq_char(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(args[0].type == VALUE_STRING && args[1].type == VALUE_STRING,
         "Eq expects char arguments");

  ASSERT(strlen(args[0].string_value) == 1 && strlen(args[1].string_value) == 1,
         "Eq expects char arguments");

  char x = args[0].string_value[0];
  char y = args[1].string_value[0];

  return MAKE_INTEGER(x == y);
}

Value str_slice(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 3, "Expected 3 arguments, but got %zu", argc);

  char* str = args[0].string_value;
  int start = args[1].int_value;
  int end = args[2].int_value;

  int len = strlen(str);

  size_t final_len = end - start;
  if (final_len < 0) final_len = 0;

  char* slice = malloc((final_len + 1) * sizeof(char));
  strncpy(slice, str + start, final_len);

  return MAKE_STRING(slice);
}

Value ffi_slice_list(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 3, "Expected 3 arguments, but got %zu", argc);

  Value list = args[0];
  int start = args[1].int_value;
  int end = args[2].int_value;

  ValueList new_list;
  new_list.length = end - start;
  new_list.values = malloc(new_list.length * sizeof(Value));

  for (int i = 0; i < new_list.length; i++) {
    new_list.values[i] = list.list_value.values[start + i];
  }

  return MAKE_LIST(new_list);
}