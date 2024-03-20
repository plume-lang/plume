#ifndef BYTECODE_H
#define BYTECODE_H

#include <stdint.h>

#include "value.h"

typedef enum {
  OP_LoadLocal,
  OP_StoreLocal,
  OP_CLoad,
  OP_FLoad,
  OP_LoadGlobal,
  OP_StoreGlobal,
  OP_Return,
  OP_Compare,
  OP_And,
  OP_Or,
  OP_NLoad,
  OP_MakeList,
  OP_ListGet,
  OP_Call,
  OP_Jump,
  OP_JumpIf,
  OP_JumpIfRel,
  OP_TypeOf,
  OP_ConstructorName,
  OP_Phi,
  OP_MakeLambda
} PlumeOpcode;

typedef struct {
  uint8_t operand1;
  uint8_t operand2;
  PlumeOpcode opcode;
} PlumeInstruction;

typedef struct {
  PlumeInstruction* instructions;
  int size;
} PlumeBytecode;

typedef Value* PlumeConstants;

typedef struct {
  PlumeConstants constants;
  int size;
} PlumeConstantsTable;

typedef struct {
  int arity;
  int address;
  int local_space;
} PlumeFunction;

typedef struct {
  PlumeFunction* functions;
  int size;
} PlumeFunctionTable;

typedef struct {
  PlumeBytecode bytecode;
  PlumeConstantsTable constants;
  PlumeFunctionTable functions;
} PlumeModule;

#endif  // BYTECODE_H