
__attribute__ ((noinline)) int nondet() { return 0; }


int smp_found_config;


void enable_IR_x2apic(void)
{
  static char c;
  static char *ptr = &c;

  *ptr = 'a';
  if (ptr)
    ptr = (void*)0;
}

int APIC_init_uniprocessor(void)
{
  enable_IR_x2apic();
}


int smp_sanity_check()
{
  if (!smp_found_config)
    APIC_init_uniprocessor();
  return 0;
}

void native_smp_prepare_cpus()
{
  enable_IR_x2apic();
  smp_sanity_check();
}


int main(void)
{

  smp_found_config = nondet();
  native_smp_prepare_cpus();

  return 0;
}
