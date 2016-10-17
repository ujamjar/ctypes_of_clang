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

enum _qoo {
  AA = 'a',
  BB = 'c'
};

enum _poo {
  AAA = -7,
  BBB = 8
};

enum {
  ARGH, GROO
};

struct y;

static void yoo(void);

