struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }

struct pch_udc_dev {
	struct spinlock lock;
};

void pch_udc_svc_cfg_interrupt(struct pch_udc_dev *dev)
{
	int i;

	spin_lock(&dev->lock);

	for (i = 0; i < 8; i++) {
		nondet();
	}

	spin_lock(&dev->lock);
}
