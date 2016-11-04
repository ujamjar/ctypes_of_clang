struct foo {
  int a;
  struct {
    float b;
  } c;
};

extern struct foo init_foo(int, float);
extern void print_foo(struct foo);
extern void add_foo_ptr(struct foo *, struct foo *, struct foo *);
extern struct foo add_foo(struct foo, struct foo);
//extern struct foo map_foo_a(struct foo, int(*)(int)); // FIXME
//extern int incr(int);

