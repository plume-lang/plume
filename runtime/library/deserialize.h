#ifndef DESERIALIZE_H
#define DESERIALIZE_H

#include <stdio.h>

#include "bytecode.h"

PlumeModule deserialize(FILE *fp);

#endif  // DESERIALIZE_H