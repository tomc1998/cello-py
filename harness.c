#include <stdio.h>

struct Foo {
  int a;
  int b;
};

int other_add_foo(struct Foo*);

int main(int argc, char** argv) {
  struct Foo f;
  f.a = 3;
  f.b = 4;
  printf("Exp 7: %d\n", other_add_foo(&f));
}
