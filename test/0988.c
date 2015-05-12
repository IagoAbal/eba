
extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));

__attribute__ ((noinline)) int nondet() { return 42; }

int vlan_hwaccel_do_receive()
{
   __assert_fail ("0", "0988c4c.c", 16, __PRETTY_FUNCTION__);
  return 0;
}


int __netif_receive_skb()
{
  if (nondet())
 {
   vlan_hwaccel_do_receive();
 }
  return 0;
}

int netif_receive_skb()
{
  return __netif_receive_skb();
}

int main(void)
{
  netif_receive_skb();
  return 0;
}
