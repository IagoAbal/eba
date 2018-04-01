struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }


void __blkdev_get(struct spinlock lock, int for_part)
{
	if (!for_part) return;

	spin_lock(&lock);

	if (__builtin_expect((long )(! (! for_part)), 0L)) return;

	spin_lock(&lock);
}