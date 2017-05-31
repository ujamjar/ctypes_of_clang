struct a {
  int a;
};

struct b {
  int b;
};

struct c {
  int c;
  union {
    struct a a;
    struct b b;
  };
};
