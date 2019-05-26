#include <stdio.h>

struct Foo {
  int a;
  int b;
};

int __mangle_add_foopi32i32(struct Foo* f);
int __mangle_add_i32i32i32(int, int);
void __mangle_clone_foopi32i32pi32i32(struct Foo* f, struct Foo* target);
int __mangle_derefpi32(int*);
int __mangle_get_foo_api32i32(struct Foo* f);
struct Foo* __mangle_get_foo_ptrpi32i32(struct Foo* f);

int main(int argc, char** argv) {
  struct Foo f;
  struct Foo clone;
  clone.a = 0;
  clone.b = 0;
  int a = 5;
  printf("Function returns same pointer: %p == %p\n", &f, __mangle_get_foo_ptrpi32i32(&f));
  printf("Deref (Exp. %d): %d\n", a, __mangle_derefpi32(&a));
  printf("Get A (Exp. %d): %d\n", f.a, __mangle_get_foo_api32i32(&f));
  printf("Add Foo (Exp. %d): %d\n", f.a + f.b, __mangle_add_foopi32i32(&f));
  __mangle_clone_foopi32i32pi32i32(&f, &clone);
  printf("Cloned Foo a = %d (exp %d), b = %d (exp %d)\n", clone.a, f.a, clone.b, f.b);
  printf("%d + %d = %d\n", 2, 3, __mangle_add_i32i32i32(2, 3));
}
