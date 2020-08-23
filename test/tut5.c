
struct foo {
  int *a, b, *c;
};

struct bar {
  struct foo f;
  int *a, b;
};

struct baz {
  struct bar b;
  int a, *c;
};

int main()
{
  struct baz b[37];
  int i;

  for (i = 0; i < 37; i++) {
    b[i].a = 3;
  }

  return 0;
}
