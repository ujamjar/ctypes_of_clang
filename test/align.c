#include <stdio.h>

// playground for learning about alignment rules.  
// They be more complex than I thought.

struct x1 { // size=1 align=1
  char p[1];
};

struct x2 { // size=4 align=2(?)
  char p;
  short q;
};

struct x3 { // size=8(?) align=4
  int p;
  unsigned char q[1];
}; 

struct x4 { // size=6 align=2
  short d[3];
};

void main(void) {

  printf("size=%li align=%li\n", sizeof(struct x1), __alignof__(struct x1));
  printf("size=%li align=%li\n", sizeof(struct x2), __alignof__(struct x2));
  printf("size=%li align=%li\n", sizeof(struct x3), __alignof__(struct x3));
  printf("size=%li align=%li\n", sizeof(struct x4), __alignof__(struct x4));

}

