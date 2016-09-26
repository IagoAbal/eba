struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }

struct ivtv {
	struct spinlock dma_reg_lock;
};

void ivtv_irq_handler(int irq, void *dev_id)
{
	struct ivtv *itv = (struct ivtv *)dev_id;
	unsigned int combo;
	unsigned int vsync_force = 0;

	spin_lock(&itv->dma_reg_lock);

	if (combo) ;

	if (0 == combo) {
		if (!vsync_force) {
			spin_unlock(&itv->dma_reg_lock);
			return;
		}
	}

	if (!combo)
		spin_lock(&itv->dma_reg_lock);

}
