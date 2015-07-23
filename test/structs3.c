
struct bar;

struct foo {
  int x;
  struct bar *b;
};

struct bar {
  int y;
  struct foo *a;
};

int main(void) {
  struct foo s;
  return 0;
}
