#include <stdio.h>

struct Foo {
  int a;
  int b;
};

struct Foo* get_foo_ptr(struct Foo* f);

int main(int argc, char** argv) {
  struct Foo f;
  printf("Function returns same pointer: %p == %p\n", &f, get_foo_ptr(&f));
}
