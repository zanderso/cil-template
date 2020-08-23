
#include <stdio.h>

int deleted = 37;

int target()
{
  int l;
  deleted = 0;
  l = deleted;
  return l;
}

int main()
{
  int r;
  r = target();
  printf("r = %d\n", r);
  return 0;
}
