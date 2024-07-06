#include <stdio.h>
#include <stdlib.h>

char *int_to_str(int n) {
  char *str = (char *)malloc(10);
  sprintf(str, "%d", n);

  return str;
}

void* unreachable() {
  printf("Unreachable code reached\n");
  exit(1);
}