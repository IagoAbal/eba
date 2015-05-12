
extern void *malloc (long unsigned int __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

unsigned char *wbuf; /* Write-behind buffer for NAND flash */
unsigned char *wbuf_verify; /* read-back buffer for verification */

int jffs2_nor_wbuf_flash_setup() {
  wbuf = malloc(32);
  if (!wbuf)
    return -12;

  return 0;
}

static int jffs2_verify_write()
{
  return(int) *wbuf_verify; // ERROR (6)
}

int __jffs2_flush_wbuf()
{
  jffs2_verify_write(); // (5)
  return 0;
}

int jffs2_flash_writev()
{
  __jffs2_flush_wbuf(); // (4)
  return 0;
}

int jffs2_flash_write()
{
  return jffs2_flash_writev(); // (3)
}

int main()
{
  jffs2_nor_wbuf_flash_setup(); // (1)
  jffs2_flash_write(); // (2)
  return 0;
}
