#include <stdint.h>
#include <stdio.h>

int64_t facto(int64_t n) {
  if (n == 0) {
    return 1;
  }
  return n * facto(n - 1);
}

int main() {
  printf("%lld\n", facto(20));
  return 0;
}