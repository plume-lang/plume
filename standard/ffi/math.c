#include <core/error.h>
#include <math.h>
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

Value add_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(args[0].float_value + args[1].float_value);
}

Value sub_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Sub expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(args[0].float_value - args[1].float_value);
}

Value mul_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(args[0].float_value * args[1].float_value);
}

Value div_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Div expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(args[0].float_value / args[1].float_value);
}

Value mod_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mod expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(fmod(args[0].float_value, args[1].float_value));
}

Value pow_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Pow expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Sub expects float arguments");

  return MAKE_FLOAT(pow(args[0].float_value, args[1].float_value));
}

Value float_to_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("FloatToInt expects 1 argument");
  ASSERT(args[0].type == VALUE_FLOAT, "FloatToInt expects a float argument");

  return MAKE_INTEGER((int)args[0].float_value);
}

Value int_to_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("IntToFloat expects 1 argument");
  ASSERT(args[0].type == VALUE_INT, "IntToFloat expects an integer argument");

  return MAKE_FLOAT((float)args[0].int_value);
}

Value eq_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Eq expects integer arguments");

  return MAKE_INTEGER(args[0].int_value == args[1].int_value);
}

Value eq_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Eq expects float arguments");

  return MAKE_INTEGER(args[0].float_value == args[1].float_value);
}

Value lt_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Lt expects 2 arguments");
  ASSERT(args[0].type == VALUE_INT && args[1].type == VALUE_INT,
         "Lt expects integer arguments");

  return MAKE_INTEGER(args[0].int_value < args[1].int_value);
}

Value lt_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Lt expects 2 arguments");
  ASSERT(args[0].type == VALUE_FLOAT && args[1].type == VALUE_FLOAT,
         "Lt expects float arguments");

  return MAKE_INTEGER(args[0].float_value < args[1].float_value);
}