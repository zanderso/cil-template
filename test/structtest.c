#include <stdio.h>

struct argtest {
  char *small;
  char *help;
  int def;
  void *req;
} __attribute__((ciltutarg)) purple = {
  "-p", "The color of purple", 6, (void *__attribute__((ciltut_assert(purple > 5))))0
};


int main()
{
  printf("%s\n", purple.help);
  return 0;
}
