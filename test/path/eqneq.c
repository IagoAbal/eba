struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }


void pch_udc_svc_cfg_interrupt(struct pch_udc_dev *dev)
{
	struct spinlock lock;
	int x;

	spin_lock(&lock);

	if (x == 12345)
	if (x != 12345)
		spin_lock(&lock);
}