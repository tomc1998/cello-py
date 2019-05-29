#include <stdio.h>

int add(int a, int b, int c) {
  return a + b + c;
}

int __mangle_cello_add(int, int, int);

int main(int argc, char** argv) {
  printf("%d\n", __mangle_cello_add(2, 3, 4));
}
