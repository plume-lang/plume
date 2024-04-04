#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

#include "cons.h"

Value add_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT(args[0].type == VALUE_STRING && args[1].type == VALUE_STRING,
         "Add expects string arguments");

  char* new_str =
      malloc(strlen(args[0].string_value) + strlen(args[1].string_value) + 1);
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

  char* new_str = malloc(100);
  switch (args[0].type) {
    case VALUE_INT:
      sprintf(new_str, "%lld", args[0].int_value);
      break;
    case VALUE_FLOAT:
      sprintf(new_str, "%f", args[0].float_value);
      break;
    case VALUE_STRING:
      sprintf(new_str, "\"%s\"", args[0].string_value);
      break;
    case VALUE_LIST:
      sprintf(new_str, "[");
      for (int i = 0; i < args[0].list_value.length; i++) {
        strcat(new_str, args[0].list_value.values[i].string_value);
        if (i < args[0].list_value.length - 1) strcat(new_str, ", ");
      }
      strcat(new_str, "]");
      break;
    case VALUE_ADDRESS:
      sprintf(new_str, "<function 0x%x>", args[0].address_value);
      break;
    case VALUE_NATIVE:
      sprintf(new_str, "<native>");
      break;
    case VALUE_SPECIAL:
      sprintf(new_str, "<special>");
      break;
  }

  return MAKE_STRING(new_str);
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
