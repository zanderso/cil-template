#include <stdio.h>
#include <stdlib.h>

void tut_color_le(int r1, int g1, int b1,
                  int r2, int g2, int b2,
                  char const *f, int l)
{
  if (r1 > r2 || g1 > g2 || b1 > b2) {
    fprintf(stderr,"%s:%d Bad color coercion: (%d,%d,%d) > (%d,%d,%d)\n",
            f, l, r1, g1, b1, r2, g2, b2);
    exit (-1);
  }
  return;
}

void tut_color_eq(int r1, int g1, int b1,
                  int r2, int g2, int b2,
                  char const *f, int l)
{
  if (r1 != r2 || g1 != g2 || b1 != b2) {
    fprintf(stderr,"%s:%d Bad color coercion: (%d,%d,%d) != (%d,%d,%d)\n",
            f, l, r1, g1, b1, r2, g2, b2);
    exit (-1);
  }
  return;
}
