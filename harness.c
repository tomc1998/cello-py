#include <stdio.h>

int __mangle_sum_natural(int);
int __mangle_next_num();

int main(int argc, char** argv) {
  printf("Exp sum of natural numbers up to 5 (15): %d\n", __mangle_sum_natural(5));
  int n0 = __mangle_next_num();
  int n1 = __mangle_next_num();
  int n2 = __mangle_next_num();
  printf("Exp 1, 2, 3: %d, %d, %d\n", n0, n1, n2);
}
