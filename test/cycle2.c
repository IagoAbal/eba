
int main(void)
{
  int a;
  int *b = &a;

  // here the the cast int* -> int doesn't cause the cycle, but the
  // the assignment `z <~ ptr ref z' can we read as that we have a
  // reference (potentially) holding a pointer to itself.
  *b = b;

  return 0;
}
