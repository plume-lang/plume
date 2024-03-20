#include "deserialize.h"

#include <stdio.h>
#include <stdlib.h>

#include "bytecode.h"
#include "value.h"

PlumeInstruction decode_opcode(FILE *fp) {
  PlumeInstruction instr;
  uint8_t opcode;
  fread(&opcode, sizeof(uint8_t), 1, fp);
  int64_t op1 = 0;
  int64_t op2 = 0;

  switch (opcode) {
    case OP_Return:
    case OP_And:
    case OP_Or:
    case OP_TypeOf:
    case OP_ConstructorName:
      break;
    default: {
      fread(&op1, sizeof(int64_t), 1, fp);
      switch (opcode) {
        case OP_Phi: {
          fread(&op2, sizeof(int64_t), 1, fp);
          break;
        }
      }
      break;
    }
  }
  instr.opcode = opcode;
  instr.operand1 = op1;
  instr.operand2 = op2;

  return instr;
}

PlumeBytecode deserialize_instructions(FILE *fp) {
  PlumeBytecode bc;
  int64_t size;
  fread(&size, sizeof(int64_t), 1, fp);
  PlumeInstruction *instructions = malloc(size * sizeof(PlumeInstruction));

  for (int i = 0; i < size; i++) {
    instructions[i] = decode_opcode(fp);
  }

  bc.instructions = instructions;
  bc.size = size;
  return bc;
}

Value deserialize_value(FILE *fp) {
  uint8_t type;
  fread(&type, sizeof(uint8_t), 1, fp);
  Value v;
  v.type = type;
  switch (type) {
    case INTEGER: {
      int64_t i;
      fread(&i, sizeof(int64_t), 1, fp);
      v.data.integer = i;
      break;
    }
    case FLOAT: {
      double f;
      fread(&f, sizeof(double), 1, fp);
      v.data.real = f;
      break;
    }
    case STRING: {
      int64_t size;
      fread(&size, sizeof(int64_t), 1, fp);
      char *s = malloc(size * sizeof(char));
      fread(s, sizeof(char), size, fp);
      v.data.string = s;
      break;
    }
  }
  return v;
}

PlumeConstantsTable deserialize_constants(FILE *fp) {
  PlumeConstantsTable ct;
  int64_t size;
  fread(&size, sizeof(int64_t), 1, fp);
  PlumeConstants constants = malloc(size * sizeof(PlumeConstants));

  for (int i = 0; i < size; i++) {
    constants[i] = deserialize_value(fp);
  }

  ct.constants = constants;
  ct.size = size;
  return ct;
}

PlumeFunctionTable deserialize_functions(FILE *fp) {
  PlumeFunctionTable ft;
  int64_t size;
  fread(&size, sizeof(int64_t), 1, fp);
  PlumeFunction *functions = malloc(size * sizeof(PlumeFunction));

  for (int i = 0; i < size; i++) {
    int64_t arity, address, local_space;
    fread(&arity, sizeof(int64_t), 1, fp);
    fread(&address, sizeof(int64_t), 1, fp);
    fread(&local_space, sizeof(int64_t), 1, fp);

    functions[i] = (PlumeFunction){arity, address, local_space};
  }

  ft.functions = functions;
  ft.size = size;
  return ft;
}

PlumeModule deserialize(FILE *fp) {
  PlumeBytecode bc = deserialize_instructions(fp);
  PlumeConstantsTable ct = deserialize_constants(fp);
  PlumeFunctionTable ft = deserialize_functions(fp);

  return (PlumeModule){bc, ct, ft};
}