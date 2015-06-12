extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));

__attribute__ ((noinline)) int nondet() { return 42; }

static int* _omap3_sram_configure_core_dpll;

int omap3_configure_core_dpll()
{
 ((_omap3_sram_configure_core_dpll)
 ? (void) (0)
 : __assert_fail ("_omap3_sram_configure_core_dpll", "63878ac.c", 16, __PRETTY_FUNCTION__));
 return *_omap3_sram_configure_core_dpll;
}

int omap3_core_dpll_m2_set_rate()
{
 omap3_configure_core_dpll();
 return 0;
}

int _omap2_init_reprogram_sdrc(void)
{
 int v;

 v = omap3_core_dpll_m2_set_rate();

 return v;
}


int omap34xx_sram_init(void)
{
 return 0;
}

int omap_sram_init(void)
{
 if (nondet())
  omap34xx_sram_init();

 return 0;
}

void omap_sdrc_init()
{
 omap_sram_init();

 if (nondet()) {

  _omap2_init_reprogram_sdrc();

 }
}

void omap3pandora_init(void)
{
 omap_sdrc_init();
}

int main(void)
{
 omap3pandora_init();
 return 0;
}
