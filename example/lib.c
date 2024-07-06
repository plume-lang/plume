#include <stdio.h>
#include <stdlib.h>

char *int_to_str(int n) {
  char *str = (char *)malloc(10);
  sprintf(str, "%d", n);

  return str;
}

void ffi_print(char* str) {
  printf("%s\n", str);
}

void ffi_println(char* str) {
  printf("%s\n", str);
}