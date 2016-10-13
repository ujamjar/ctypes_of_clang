extern void foo(int a);

struct _goo {
  int a;
  char *b;
};

typedef struct _goo roo;

union _woo {
  long long c;
  void *d[3];
};

enum _boo {
  A = 10, B = 20
};

typedef enum coo {
  x,y,z
} doo;


