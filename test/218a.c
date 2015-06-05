
extern void *malloc (unsigned long __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

int *x = (void*)0;
unsigned int size = 12 +1;

int inet_ehash_locks_alloc()
{
  if (size > 12)
	x = malloc(size);
  else
	x = malloc(size);
  if (!x)
	return 12;
  return 0;
}

void inet_ehash_locks_free()
{
  if (x) {
    if (size > 12)
      free(x);
    else
	  x = (void*)0;
  }
}

int main(void)
{
  inet_ehash_locks_alloc();
  inet_ehash_locks_free();
  return 0;
}
