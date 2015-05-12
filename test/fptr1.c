
int (*f)(int);

int rand() { return 42; }

int ident(int x) {
  return x;
}

int inc(int x) {
  return ++x;
}

int main() {
  if (rand()) {
    f = &ident;
  } else {
    f = &inc;
  }
  f(0);
}
