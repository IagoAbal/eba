
int main(void)
{
  int a;
  int *b = &a;

  // here the cycle comes from the implicit cast: (int *)*b
  // which can be reduced to a cyclic coercion: int -> int*
  // this can be detected given the direction of unification:
  // `ptr ref z <~ z' means that any shape `z' can be infinitely
  // coerced into a pointer-shape to itself.
  b = *b;

  return 0;
}
