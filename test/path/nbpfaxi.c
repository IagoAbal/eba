struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }

struct nbpf_channel {
	struct spinlock lock;
};

void nbpf_chan_tasklet(unsigned long data)
{
	struct nbpf_channel *chan = (struct nbpf_channel *)data;

	while (!nondet()) {
		_Bool found = 0, recycling = 0;

		spin_lock(&chan->lock);

		while (nondet()) {
			if (nondet()) {
				found = 1;
				break;
			} else if (nondet()) {
				spin_unlock(&chan->lock);
				recycling = 1;
				break;
			}
		}

		if (recycling)
			continue;

		if (!found) {
			spin_unlock(&chan->lock);
			break;
		}

		spin_unlock(&chan->lock);

	}
}

