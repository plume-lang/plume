#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "library/deserialize.h"

int catch (char *fmt, ...)
{
  va_list(args);
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);

  exit(EXIT_FAILURE);

  return 0;
}

int main(int argc, char **argv)
{
  if (argc < 2)
    return catch ("Usage: %s <file_name>\n", argv[0]);
  char *file_name = argv[1];
  FILE *fp = fopen(file_name, "rb");
  if (fp == NULL)
    return catch ("Error: Unable to open file %s\n", file_name);

  deserialize(fp);

  printf("%s\n", file_name);
}
