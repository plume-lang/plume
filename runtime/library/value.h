#ifndef VALUE_H
#define VALUE_H

typedef enum {
  INTEGER,
  FLOAT,
  STRING,
  ADDR,
  NATIVE_ADDR,
  LIST,
} ValueType;

typedef struct Value {
  int type;
  union {
    int integer;
    float real;
    char *string;
    int addr;
    int native_addr;
    struct Value *list;
  } data;
} Value;

#endif  // VALUE_H