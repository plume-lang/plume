#include <core/error.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

Value add_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Sub expects integer arguments");

  return MAKE_INTEGER(args[0].int_value + args[1].int_value);
}

Value sub_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Sub expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Sub expects integer arguments");

  return MAKE_INTEGER(args[0].int_value - args[1].int_value);
}

Value mul_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Sub expects integer arguments");

  return MAKE_INTEGER(args[0].int_value * args[1].int_value);
}

Value div_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Div expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Sub expects integer arguments");

  return MAKE_INTEGER(args[0].int_value / args[1].int_value);
}

Value mod_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mod expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Sub expects integer arguments");

  return MAKE_INTEGER(args[0].int_value % args[1].int_value);
}