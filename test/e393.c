
__attribute__ ((noinline)) int nondet() { return 42; }

int netpoll_setup()
{
  int err;

  goto put; // (2)

put:
  return err; // ERROR (3)
}

int main(void)
{
  netpoll_setup(); // (1)
  return 0;
}
