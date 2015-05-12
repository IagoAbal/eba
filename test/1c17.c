
extern int printf (const char *__restrict __format, ...);

void print_cpu_stall_fast_no_hz(char *cp, int cpu)
{
}

void print_cpu_stall_info(int cpu)
{
	char fast_no_hz[72];

	print_cpu_stall_fast_no_hz(fast_no_hz, cpu);
	printf("\t%d: %s\n", cpu, fast_no_hz); // ERROR
}

int main(void) {
  print_cpu_stall_info(0);
  return 0;
}
