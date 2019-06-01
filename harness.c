#include <stdio.h>

int __mangle_sum_natural(int);

int main(int argc, char** argv) {
  printf("Exp sum of natural numbers up to 5 (15): %d\n", __mangle_sum_natural(5));
}
