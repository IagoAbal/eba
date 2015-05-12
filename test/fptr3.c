
void* f() {
  return (void *)&f;
}

int main() {
  f();
  return 0;
}
