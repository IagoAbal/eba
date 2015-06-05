
extern void *malloc (unsigned long __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

__attribute__ ((noinline)) int nondet() { return 42; }

extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       unsigned long __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




int load_module()
{
  void *hdr;
  void **mod;
  long err = 0;
  void *ptr = (void*)0;

  if ((hdr = malloc(2*sizeof(void*))) == (void*)0)
    return -12;

  mod = (void *)hdr;


  (mod[0]) = malloc(32);
  if (!(mod[0]))
  {
    err = -12;
    goto free_mod;
  }


  ptr = malloc(512);
  if (!ptr) {
    err = -12;
    goto free_percpu;
  }
  (mod[1]) = ptr;

  while (nondet()) {
    void *dest = (mod[1]);
    memcpy(dest, mod, sizeof(2*sizeof(void*)));
    mod = (mod[1]);
    if (nondet())
      break;
  }

  if (nondet())
    goto free_unload;

  return 0;

  free_unload:
  free_core:
    free((mod[1]));
  free_percpu:

    free((mod[0]));

  free_mod:
  free_hdr:
    free(hdr);

  return err;
}

int main(void) {
  load_module();
  return 0;
}
