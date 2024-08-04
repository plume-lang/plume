#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

#include "cons.h"

Value add_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT_FMT(get_type(args[0]) == TYPE_STRING && get_type(args[1]) == TYPE_STRING,
             "Add expects string arguments, received %s and %s", type_of(args[0]),
             type_of(args[1]));

  HeapValue* str1 = GET_PTR(args[0]);
  HeapValue* str2 = GET_PTR(args[1]);

  char* new_str =
      malloc(str1->length + str2->length + 1);
  strcpy(new_str, str1->as_string);
  strcat(new_str, str2->as_string);

  return MAKE_STRING(new_str);
}

Value mul_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_STRING && get_type(args[1]) == TYPE_INTEGER,
         "Mul expects string and integer arguments");

  HeapValue* str = GET_PTR(args[0]);

  char* new_str = malloc(str->length * GET_INT(args[1]) + 1);
  new_str[0] = '\0';

  for (int i = 0; i < GET_INT(args[1]); i++) {
    strcat(new_str, GET_STRING(args[0]));
  }

  return MAKE_STRING(new_str);
}

Value int_to_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("Int_to_str expects 1 argument");
  ASSERT(get_type(args[0]) == TYPE_INTEGER,
         "Int_to_str expects an integer argument");

  int x = GET_INT(args[0]);
  int length = snprintf(NULL, 0, "%d", x);
  char* str = malloc(length + 1);

  snprintf(str, length + 1, "%d", x);

  return MAKE_STRING(str);
}

Value to_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("To_string expects 1 argument");
  switch (get_type(args[0])) {
    case TYPE_MUTABLE: {
      Value v = GET_MUTABLE(args[0]);
      HeapValue* str = GET_PTR(to_string(1, mod, &v));
      size_t sz = str->length + 5;
      char* new_str = malloc(sz * sizeof(char));
      strcpy(new_str, "mut ");
      strcpy(new_str + 4, str->as_string);
      return MAKE_STRING(new_str);
    }
    case TYPE_INTEGER: {
      size_t sz = snprintf(NULL, 0, "%d", (int32_t) GET_INT(args[0]));
      char* new_str = malloc((sz + 1) * sizeof(char));
      sprintf(new_str, "%d", (int32_t) args[0]);
      return MAKE_STRING(new_str);
    }
    case TYPE_FLOAT: {
      size_t sz = snprintf(NULL, 0, "%f", GET_FLOAT(args[0]));
      char* new_str = malloc((sz + 1) * sizeof(char));
      sprintf(new_str, "%f", GET_FLOAT(args[0]));
      return MAKE_STRING(new_str);
    }
    case TYPE_STRING: {
      HeapValue* hp = GET_PTR(args[0]);
      char* str = hp->as_string;
      size_t sz = hp->length;
      char* new_str = malloc(sz + 3);
      new_str[0] = '"';
      strcpy(new_str + 1, str);
      new_str[sz + 1] = '"';
      return MAKE_STRING(new_str);
    }
    case TYPE_LIST: {
      size_t sz = 2;
      HeapValue *hp = GET_PTR(args[0]);
      char* res = "[";
      for (int i = 0; i < hp->length; i++) {
        HeapValue* new_hp = GET_PTR(to_string(1, mod, &hp->as_ptr[i]));
        sz += new_hp->length;
        res = realloc(res, sz);
        strcat(res, new_hp->as_string);
        if (i < hp->length - 1) {
          sz += 2;
          res = realloc(res, sz);
          strcat(res, ", ");
        }
      }

      sz += 2;
      res = realloc(res, sz);
      strcat(res, "]");

      return MAKE_STRING(res);
    }
    case TYPE_SPECIAL: {
      return MAKE_STRING("<special>");
    }
    case TYPE_FUNCTION: case TYPE_FUNCENV: {
      return MAKE_STRING("<function>");
    }
    case TYPE_UNKNOWN: {
      return MAKE_STRING("<unknown>");
    }
  }

  return MAKE_STRING("<unknown>");
}

Value string_length(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("String_length expects 1 argument");
  ASSERT(get_type(args[0]) == TYPE_STRING,
         "String_length expects a string argument");

  HeapValue* str = GET_PTR(args[0]);
  return MAKE_INTEGER(strlen(str->as_string));
}

Value eq_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_STRING && get_type(args[1]) == TYPE_STRING,
         "Eq expects string arguments");

  HeapValue* str1 = GET_PTR(args[0]);
  HeapValue* str2 = GET_PTR(args[1]);

  return MAKE_INTEGER(strcmp(str1->as_string, str2->as_string) == 0);
}

Value list_append(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_append expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_LIST,
         "List_append expects a list as the first argument");

  Value list = args[0];
  Value value = args[1];

  HeapValue* hp = GET_PTR(list);

  Value* new_list = malloc((hp->length + 1) * sizeof(Value));

  for (int i = 0; i < hp->length; i++) {
    new_list[i] = hp->as_ptr[i];
  }

  new_list[hp->length] = value;

  return MAKE_LIST(new_list, hp->length + 1);
}

Value list_prepend(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_prepend expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_LIST,
         "List_prepend expects a list as the first argument");

  Value list = args[0];
  Value value = args[1];

  HeapValue* hp = GET_PTR(list);

  Value* new_list = malloc((hp->length + 1) * sizeof(Value));

  new_list[0] = value;
  for (int i = 0; i < hp->length; i++) {
    new_list[i + 1] = hp->as_ptr[i];
  }

  return MAKE_LIST(new_list, hp->length + 1);
}

Value list_concat(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("List_concat expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_LIST && get_type(args[1]) == TYPE_LIST,
         "List_concat expects list arguments");

  HeapValue* hp1 = GET_PTR(args[0]);
  HeapValue* hp2 = GET_PTR(args[1]);

  Value* new_list = malloc((hp1->length + hp2->length) * sizeof(Value));

  size_t sz = hp1->length;

  for (int i = 0; i < hp1->length; i++) {
    new_list[i] = hp1->as_ptr[i];
  }

  for (int i = 0; i < hp2->length; i++) {
    new_list[i + sz] = hp2->as_ptr[i];
  }

  return MAKE_LIST(new_list, hp1->length + hp2->length);
}

Value get_index_str(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("GetIndex expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_STRING && get_type(args[1]) == TYPE_INTEGER,
         "GetIndex expects string and integer arguments");

  int idx = GET_INT(args[1]);
  HeapValue* hp = GET_PTR(args[0]);
  char* str = hp->as_string;

  if (idx < 0 || idx >= hp->length) return make_none(mod->gc);

  return make_some(mod->gc, MAKE_CHAR(mod->gc, str[idx]));
}

Value char_to_string(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("CharToString expects 1 argument");
  ASSERT_FMT(get_type(args[0]) == TYPE_STRING,
         "CharToString expects a string argument, received %s", type_of(args[0]));

  return args[0];
}

Value eq_char(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT_FMT(get_type(args[0]) == TYPE_STRING && get_type(args[1]) == TYPE_STRING,
         "Eq expects char arguments, received %s and %s", type_of(args[0]), type_of(args[1]));

  HeapValue* hp1 = GET_PTR(args[0]);
  HeapValue* hp2 = GET_PTR(args[1]);

  ASSERT(hp1->length == 1 && hp2->length == 1,
         "Eq expects char arguments");

  char x = hp1->as_string[0];
  char y = hp2->as_string[0];

  return MAKE_INTEGER(x == y);
}

Value str_slice(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 3, "Expected 3 arguments, but got %zu", argc);

  HeapValue* hp = GET_PTR(args[0]);

  char* str = hp->as_string;
  int start = GET_INT(args[1]);
  int end = GET_INT(args[2]);

  size_t len = hp->length;

  size_t final_len = end - start;
  if (final_len < 0) final_len = 0;

  char* slice = malloc((final_len + 1) * sizeof(char));
  strncpy(slice, str + start, final_len);

  return MAKE_STRING(slice);
}

Value ffi_slice_list(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 3, "Expected 3 arguments, but got %zu", argc);

  HeapValue* list = GET_PTR(args[0]);
  Value* list_values = list->as_ptr;
  int start = GET_INT(args[1]);
  int end = GET_INT(args[2]);

  size_t sz = end - start;
  Value* new_list = malloc(sz * sizeof(Value));

  for (int i = 0; i < sz; i++) {
    new_list[i] = list_values[start + i];
  }

  return MAKE_LIST(new_list, sz);
}