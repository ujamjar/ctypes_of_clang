#include <stdio.h>
#include "foo.h"

struct foo init_foo(int a, float c) {
  struct foo foo = { a, { c } };
  return foo;
}

void print_foo(struct foo foo) {
  printf("foo.a = %i\n", foo.a);
  printf("foo.c.n = %f\n", foo.c.b);
}

void add_foo_ptr(struct foo *x, struct foo *y, struct foo *z) {
  z->a = x->a + y->a;
  z->c.b = x->c.b + y->c.b;
}

struct foo add_foo(struct foo a, struct foo b) {
  struct foo c;
  add_foo_ptr(&a, &b, &c);
  return c;
}

struct foo map_foo_a(struct foo a, int (*f)(int)) {
  a.a = f(a.a);
  return a;
}

int incr(int a) {
  return a + 1;
}

