
int foo(int(*f)(int*x)) {
  int a;
  f(&a);
  return a;
}
