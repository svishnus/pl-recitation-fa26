#include <stdio.h>
#define SWAP(a, b) do { \
  int temp = (a);       \
  (a) = (b);            \
  (b) = temp;           \
} while(0)

int main(void) {
  int x = 10;
  int y = 20;
  SWAP(x, y);
  printf("x = %d, y = %d\n", x, y);
  return 0;
}
