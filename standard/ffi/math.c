#include <core/error.h>
#include <math.h>
#include <module.h>
#include <stdio.h>
#include <string.h>
#include <value.h>

Value add_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Sub expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) + GET_INT(args[1]));
}

Value sub_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Sub expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Sub expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) - GET_INT(args[1]));
}

Value mul_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Sub expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) * GET_INT(args[1]));
}

Value div_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Div expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Sub expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) / GET_INT(args[1]));
}

Value mod_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mod expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Sub expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) % GET_INT(args[1]));
}

Value add_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Add expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = GET_FLOAT(args[0]) + GET_FLOAT(args[1]);
  return MAKE_FLOAT(res);
}

Value sub_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Sub expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = GET_FLOAT(args[0]) - GET_FLOAT(args[1]);
  return MAKE_FLOAT(res);
}

Value mul_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mul expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = GET_FLOAT(args[0]) * GET_FLOAT(args[1]);
  return MAKE_FLOAT(res);
}

Value div_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Div expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = GET_FLOAT(args[0]) / GET_FLOAT(args[1]);
  return MAKE_FLOAT(res);
}

Value mod_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Mod expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = fmod(GET_FLOAT(args[0]), GET_FLOAT(args[1]));
  return MAKE_FLOAT(res);
}

Value pow_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Pow expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Sub expects float arguments");

  double res = pow(GET_FLOAT(args[0]), GET_FLOAT(args[1]));
  return MAKE_FLOAT(res);
}

Value float_to_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("FloatToInt expects 1 argument");
  ASSERT(get_type(args[0]) == TYPE_FLOAT, "FloatToInt expects a float argument");

  return MAKE_INTEGER((int)GET_FLOAT(args[0]));
}

Value int_to_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 1) THROW("IntToFloat expects 1 argument");
  ASSERT(get_type(args[0]) == TYPE_INTEGER, "IntToFloat expects an integer argument");

  double res = (double)GET_INT(args[0]);
  return MAKE_FLOAT(res);
}

Value eq_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Eq expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) == GET_INT(args[1]));
}

Value eq_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Eq expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Eq expects float arguments");

  return MAKE_INTEGER(GET_FLOAT(args[0]) == GET_FLOAT(args[1]));
}

Value lt_int(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Lt expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_INTEGER && get_type(args[1]) == TYPE_INTEGER,
         "Lt expects integer arguments");

  return MAKE_INTEGER(GET_INT(args[0]) < GET_INT(args[1]));
}

Value lt_float(int arg_n, Module* mod, Value* args) {
  if (arg_n != 2) THROW("Lt expects 2 arguments");
  ASSERT(get_type(args[0]) == TYPE_FLOAT && get_type(args[1]) == TYPE_FLOAT,
         "Lt expects float arguments");

  return MAKE_INTEGER(GET_FLOAT(args[0]) < GET_FLOAT(args[1]));
}