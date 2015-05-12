
int f(int *x, int y) {
  int i;
  for (i=0;i<10;i++)
	x[i] = y;
  return x[2];
}

int g(int y) {
  int (*h)(int*,int);
  h = &f;
  return h(&y,0);
}

int main(void) {
  int x;
  f(&x,2);
  return 0;
}
