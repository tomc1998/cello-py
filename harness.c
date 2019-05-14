#include <stdio.h>

int add(int, int);
int max(int, int);
int max3(int, int, int);

int main(int argc, char** argv) {
  printf("%d\n", add(1, 2));
  printf("%d\n", max(max(1, 2), 4));
  printf("%d\n", max3(4, 2, 1));
}
