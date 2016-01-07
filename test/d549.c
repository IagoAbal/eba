
extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));

void* vlan_dev_real_dev()
{
    __assert_fail ("0", "simple/d549f55.c", 14, __PRETTY_FUNCTION__);
	return (void*)0;
}

int ocrdma_inet6addr_event()
{
	vlan_dev_real_dev(); // (2)

	return 0;
}

int main(void)
{
  ocrdma_inet6addr_event(); // (1)
  return 0;
}
