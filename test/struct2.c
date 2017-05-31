struct a {
  // field
  int a;

  // named, nested struct
  struct b {
    int b;
    struct b *xxx;  // this compiles, but how to create a struct b here?
  } b;

  // multiply named, nested struct
  struct c {
    int c, d;
  } c, d;

  // unnamed, nested struct - doesn't declare anything, 
  // but is allowed with warning -Wmissing-declaration
  struct e {
    int e;
  };

  struct {
    int f;
  } f, g;

};

struct a x={0};

int test() {
//  return x.g.e;  // error - not really in struct
return 0;
}


