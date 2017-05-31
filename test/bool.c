#include <stdio.h>
#include <stdbool.h>

struct bools {
  bool a, b;
};

void main(void) {
  printf("bool: %li %li\n", sizeof(bool), __alignof__(bool));
  printf("bools: %li %li\n", sizeof(struct bools), __alignof__(struct bools));
}
