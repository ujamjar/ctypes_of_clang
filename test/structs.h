/*struct x { 
  int a;
};

struct y {
  struct x a;
  int b;
};

struct z {
  int a;
  struct {
    int b;
    int c;
  } *d;

};

struct w {
  int j;
  enum { A, B } k;
};

union f {
  int a;
  int b;
  union _c {
    int d;
    struct _e {
      int f;
      int g;
    } e;
  } c;
};
*/
//struct x* exit (struct y );

struct a {
  int a;
  struct {
    int x;
    struct b *next;
  } c;
};

