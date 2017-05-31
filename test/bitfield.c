#include <stdio.h>

struct foo;

struct foo {
  unsigned x : 7;
  unsigned y : 3;
  unsigned z : 23;
};

int main(void) {
  struct foo foo;

  printf("sizeof=%li\n", sizeof(struct foo));
  return 0;
}

