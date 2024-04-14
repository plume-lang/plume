#include "cons.h"

#include <stdlib.h>
#include <value.h>

Value make_some(Value v) {
  ValueList l;
  l.length = 4;
  l.values = malloc(sizeof(Value) * 4);
  l.values[0] = MAKE_SPECIAL();
  l.values[1] = MAKE_STRING("Option");
  l.values[2] = MAKE_STRING("Some");
  l.values[3] = v;

  return MAKE_LIST(l);
}

Value make_unit() {
  ValueList l;
  l.length = 3;
  l.values = malloc(sizeof(Value) * 3);
  l.values[0] = MAKE_SPECIAL();
  l.values[1] = MAKE_STRING("unit");
  l.values[2] = MAKE_STRING("unit");

  return MAKE_LIST(l);
}

Value make_none() {
  ValueList l;
  l.length = 3;
  l.values = malloc(sizeof(Value) * 3);
  l.values[0] = MAKE_SPECIAL();
  l.values[1] = MAKE_STRING("Option");
  l.values[2] = MAKE_STRING("None");

  return MAKE_LIST(l);
}

Value MAKE_CHAR(char c) {
  Value v;
  v.type = VALUE_STRING;
  v.string_value = malloc(2 * sizeof(char));
  v.string_value[0] = c;
  v.string_value[1] = '\0';

  return v;
}

Value make_ok(Value v) {
  ValueList l;
  l.length = 4;
  l.values = malloc(sizeof(Value) * 4);
  l.values[0] = MAKE_SPECIAL();
  l.values[1] = MAKE_STRING("Result");
  l.values[2] = MAKE_STRING("Ok");
  l.values[3] = v;

  return MAKE_LIST(l);
}

Value make_err(Value v) {
  ValueList l;
  l.length = 4;
  l.values = malloc(sizeof(Value) * 4);
  l.values[0] = MAKE_SPECIAL();
  l.values[1] = MAKE_STRING("Result");
  l.values[2] = MAKE_STRING("Error");
  l.values[3] = v;

  return MAKE_LIST(l);
}