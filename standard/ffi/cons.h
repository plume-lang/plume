#ifndef CONS_H
#define CONS_H

#include <value.h>

Value make_some(GarbageCollector gc, Value v);
Value make_none(GarbageCollector gc);
Value MAKE_CHAR(GarbageCollector gc, char c);

Value make_unit(GarbageCollector gc);

Value make_ok(GarbageCollector gc, Value v);
Value make_err(GarbageCollector gc, Value v);

void print_helper(Value v);

#endif  // CONS_H