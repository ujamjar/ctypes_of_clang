#include <stdio.h>

void main(void) {

  long double x = 0;

  printf("%li %li %.12Lg\n", sizeof(long double), __alignof__(long double), x);
  printf("%li %li %.12Lg\n", sizeof(long double _Complex), __alignof__(long double _Complex), x);
}
