
int irq_domain_simple_ops = 1;

void irq_domain_add(int *ops)
{
  int irq = *ops;
}

int twl_probe()
{
  int *ops = (void*)0;
  irq_domain_add(ops);
}

int main()
{
  twl_probe();

  return 0;
}
