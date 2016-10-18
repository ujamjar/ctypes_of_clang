/*
int foo = 1;

enum x {
  A, B
};

enum {c,d} f(void);

//struct { int x } g(int);

struct q {
  int e;
};

struct q h();
*/

struct t {
  int a;
  struct {
    int b;
    int c;
  } d;
};
