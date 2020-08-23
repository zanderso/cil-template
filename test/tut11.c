 
#include <ciltut.h> 

void (pre(n > 0)
      post(forall(j,implies(j>=0 && j < n,*(a+j)==4)))
      arr_init)(int *a, int n)
{
  int i;

  for (i = 0; i < n; i++)
  { invariant(i != n,
              i >= 0 && i <= n && forall(j, implies(j>=0 && j<i, *(a+j) == 4)),
              i)
    a[i] = 4;
  }

  return;
}

int main()
{
  int arr[5];

  arr_init(&arr[0], 5);

  return 0;
}
