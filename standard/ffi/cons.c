#include "cons.h"

#include <stdlib.h>
#include <value.h>

Value make_some(Value v) {
  Value* values = malloc(sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Option", 6);
  values[2] = MAKE_STRING("Some", 4);
  values[3] = v;

  return MAKE_LIST(values, 4);
}

Value make_unit() {
  Value* values = malloc(sizeof(Value) * 3);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("unit", 4);
  values[2] = MAKE_STRING("unit", 4);

  return MAKE_LIST(values, 3);
}

Value make_none() {
  Value* values = malloc(sizeof(Value) * 3);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Option", 6);
  values[2] = MAKE_STRING("None", 4);

  return MAKE_LIST(values, 3);
}

Value MAKE_CHAR(char c) {
  char str[2];
  str[0] = c;
  str[1] = '\0';

  return MAKE_STRING(str, 1);
}

Value make_ok(Value v) {
  Value* values = malloc(sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Result", 6);
  values[2] = MAKE_STRING("Ok", 2);
  values[3] = v;

  return MAKE_LIST(values, 4);
}

Value make_err(Value v) {
  Value* values = malloc(sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Result", 6);
  values[2] = MAKE_STRING("Error", 5);
  values[3] = v;

  return MAKE_LIST(values, 4);
}