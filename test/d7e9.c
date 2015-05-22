
extern void spin_lock(int *);
extern void spin_unlock(int *);

__attribute__ ((noinline)) int nondet() { return 42; }

int fake_lock = 0;

int inode_get_rsv_space(int *i_lock)
{
  spin_lock(i_lock); // (4) ERROR
  // do something
  spin_unlock(i_lock);
  return 0;
}

void add_dquot_ref(int *i_lock)
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
