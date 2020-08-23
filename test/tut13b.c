extern int foo(int x);

int bar(int x)
{
  return foo(x);
}

int main()
{
  bar(1);
  return 0;
}
