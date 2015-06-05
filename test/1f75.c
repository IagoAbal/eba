
static int some_int = 0;

int* __alloc_pages_nodemask(int gfp_mask)
{

  if (gfp_mask & ((int)0x01u))
    return (void*)0;

  return &some_int;
}


int ep93xx_alloc_buffers()
{
  int *descs = __alloc_pages_nodemask((((int)0x10u) | ((int)0x40u) | ((int)0x80u)) | ((int)0x01u));
  if (descs == (void*)0)
    return 1;
}

int ep93xx_open()
{
  if (ep93xx_alloc_buffers())
    return -12;
}


int main(void)
{

  ep93xx_open();

  return 0;
}
