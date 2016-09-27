
struct spinlock;

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }

struct spinlock fake_lock;

int inode_get_rsv_space(struct spinlock *i_lock)
{
  i_lock = (struct spinlock *)1;
  spin_lock(i_lock); // (4) ERROR
  // do something
  spin_unlock(i_lock);
  return 0;
}

void add_dquot_ref(struct spinlock *i_lock)
{
  int reserved = 0;

  int i, j;
  for (i=0; i<10; i++) {
    spin_lock(i_lock); // (2) lock is acquired
    if (nondet()) { // evaluates to false
      spin_unlock(i_lock);
      continue;
    }

    if (inode_get_rsv_space(i_lock) > 0) // (3)
      reserved = 1;

    spin_unlock(i_lock);
  }
}

int main(void)
{
  add_dquot_ref(&fake_lock); // (1)
  return 0;
}
