#include "cons.h"

#include <stdlib.h>
#include <value.h>

Value make_some(Value v) {
  Value* values = gc_malloc(&gc, sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Option");
  values[2] = MAKE_STRING("Some");
  values[3] = v;

  return MAKE_LIST(values, 4);
}

Value make_unit() {
  Value* values = gc_malloc(&gc, sizeof(Value) * 3);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("unit");
  values[2] = MAKE_STRING("unit");

  return MAKE_LIST(values, 3);
}

Value make_none() {
  Value* values = gc_malloc(&gc, sizeof(Value) * 3);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Option");
  values[2] = MAKE_STRING("None");

  return MAKE_LIST(values, 3);
}

Value MAKE_CHAR(char c) {
  char* str = gc_malloc(&gc, sizeof(char) * 2);
  str[0] = c;
  str[1] = '\0';

  return MAKE_STRING(str);
}

Value make_ok(Value v) {
  Value* values = gc_malloc(&gc, sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Result");
  values[2] = MAKE_STRING("Ok");
  values[3] = v;

  return MAKE_LIST(values, 4);
}

Value make_err(Value v) {
  Value* values = gc_malloc(&gc, sizeof(Value) * 4);
  values[0] = MAKE_SPECIAL();
  values[1] = MAKE_STRING("Result");
  values[2] = MAKE_STRING("Error");
  values[3] = v;

  return MAKE_LIST(values, 4);
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

    case TYPE_FUNCTION: case TYPE_FUNCENV: {
      printf("<function>");
      break;
    }
  }
}
