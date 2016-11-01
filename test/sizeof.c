#include <stdio.h>

enum e {
  A=0,
  B=700
};

struct s {
  enum e e;
};

struct t {
  int a;
};

void main(void) {
  printf("%lu %lu %lu %lu %lu %lu %lu\n", 
      sizeof(unsigned), sizeof(struct s), sizeof(struct t), sizeof(int), 
      sizeof(va_list), sizeof(long double), sizeof(__builtin_va_list));
}

