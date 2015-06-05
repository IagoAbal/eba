
extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));

__attribute__ ((noinline)) int nondet() { return 42; }

static int* _omap3_sram_configure_core_dpll;

int omap3_configure_core_dpll()
{
  __assert_fail ("_omap3_sram_configure_core_dpll", "63878ac.c", 12, __PRETTY_FUNCTION__);
  return *_omap3_sram_configure_core_dpll;
}

int omap3_core_dpll_m2_set_rate()
{
	omap3_configure_core_dpll(); // (5)
	return 0;
}

static int _omap2_init_reprogram_sdrc(void)
{
	int v;

	v = omap3_core_dpll_m2_set_rate(); // (4)

	return v;
}

static inline int omap34xx_sram_init(void)
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
	omap_sram_init(); // does nothing in ARCH_OPAM3 && !PM

	if (nondet()) { // evaluates to true
		_omap2_init_reprogram_sdrc(); // (3)
	}
}

static void omap3pandora_init(void)
{
	omap_sdrc_init(); // (2)
}

int main(void)
{
	omap3pandora_init(); // (1)
	return 0;
}
