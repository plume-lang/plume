#ifndef CONS_H
#define CONS_H

#include <value.h>

Value make_some(Value v);
Value make_none();
Value MAKE_CHAR(char c);

#endif  // CONS_H