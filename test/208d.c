
#include <stdlib.h>
__attribute__ ((noinline)) int nondet() { return 42; }

extern void local_bh_disable(void);
extern void local_bh_enable(void);

extern void spin_lock_irq(int *lock);
extern void spin_unlock_irq(int *lock);

void kunmap_skb_frag()
{
#ifdef CONFIG_HIGHMEM
  local_bh_enable(); // ERROR
#endif
}

unsigned int skb_checksum()
{
  unsigned int csum = 0;

  while (nondet()) {
	if (nondet()) {
	  kunmap_skb_frag();
	}
  }

  return csum;
}

int udp_checksum_complete()
{
  return skb_checksum();
}

unsigned int udp_poll()
{
  int lock;
  unsigned int mask = 0;

  spin_lock_irq(&lock);
  while (nondet()) {
	udp_checksum_complete();
  }
  spin_unlock_irq(&lock);

  return mask;
}

int main(void)
{
  udp_poll();
  return 0;
}
