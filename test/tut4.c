
#include <stdio.h>

int main()
{
  int i, j;
  int c = 1;

  for (i = 0; i < 10; i++) {
    for (j = 0; j < 5; j++) {
      c *= i;
    }
  }

  return 0;
}
