
#include <stdio.h>

int bar = 37;

int foo()
{
  int l;
  bar = 0;
  l = bar;
  return l;
}

int main()
{
  int r;
  r = foo();
  printf("r = %d\n", r);
  return 0;
}
