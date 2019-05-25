#include <stdio.h>

struct Foo {
  int a;
  int b;
};

struct Foo* get_foo_ptr(struct Foo* f);
int get_foo_a(struct Foo* f);
int add_foo(struct Foo* f);
int deref(int*);

int main(int argc, char** argv) {
  struct Foo f;
  int a = 5;
  printf("Function returns same pointer: %p == %p\n", &f, get_foo_ptr(&f));
  printf("Deref (Exp. %d): %d\n", a, deref(&a));
  printf("Get A (Exp. %d): %d\n", f.a, get_foo_a(&f));
  printf("Add Foo (Exp. %d): %d\n", f.a + f.b, add_foo(&f));
}
