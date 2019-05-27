#include <stdio.h>

void __mangle_do_write();

int main(int argc, char** argv) {
  __mangle_do_write();
}
