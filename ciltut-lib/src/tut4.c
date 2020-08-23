
#include <stdio.h>

void tut_begin_loop(const char *f, int l) {}

void tut_end_loop(const char *f, int l, int c)
{
  printf("loop: %s:%d - %d times\n", f, l, c - 1);
  fflush(stdout);
  return;
}
