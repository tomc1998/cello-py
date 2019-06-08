#include <stdio.h>

struct Foo {
  int a;
  int b;
};

int extract_a(struct Foo*);

int main(int argc, char** argv) {
  struct Foo foo;
  foo.a = 6;
  printf("Exp 6: %d\n", extract_a(&foo));
}
