int kmalloc_caches[((11 + 18 - 1) <= 25 ? (11 + 18 - 1) : 25) + 1];

void init_node_lock_keys()
{
  int i;

  for (i = 1; i < 18 + 11; i++)
  {
   int* cache = &kmalloc_caches[i];

   if (!cache)
     continue;

   int n = *cache;
 }
}

void init_lock_keys(void)
{
  init_node_lock_keys();
}

void kmem_cache_init_late(void)
{
  init_lock_keys();
}

int main(void)
{
  kmem_cache_init_late();
  return 0;
}
