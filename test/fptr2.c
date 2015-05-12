
int *x;

int f(int i) {
  x = (int *)&f;
  return 0;
}

int main() {
  f(0);
  return 0;
}
