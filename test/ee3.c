
int IS_ERR(const void *ptr)
{
  return (ptr == (void *)-1);
}

int IS_ERR_OR_NULL(const void *ptr)
{
  return !ptr || IS_ERR(ptr);
}

static int some_int = 1;

int *ip6_dst_lookup_flow()
{
  return &some_int;
}

void sctp_v6_get_dst()
{
  int *dst = ((void*)0);
  dst = ip6_dst_lookup_flow();
  if (!IS_ERR(dst)) {
    dst = ((void*)0);
  }
  if (!IS_ERR(dst)) {
    char *rt =(char *) dst;
    ((*rt)++);
  }
}

int main()
{
  sctp_v6_get_dst();
  return 0;
}
