
#include <stdio.h>

int main()
{
  int a,b,c,d;
  a = 1; b = 2; c = 3; d = 4;
  a += b + c;
  c *= d - b;
  b -= d + a;
  if (a % 2) a++;
  printf("a = %d, b = %d, c = %d, d = %d\n", a, b, c, d);
  return 0;
}
