// !BUG
struct a;

struct a {
  int x;
  struct b {
    int y;
  } b;
};
