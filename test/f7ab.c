
typedef int filler_t(int);

int some_fun(int n)
{
  return n;
}

filler_t *f = ((void*)0);

filler_t* shmem_get_inode()
{
  return f;
}

filler_t* shmem_file_setup()
{
  return shmem_get_inode();
}

int drm_gem_object_init(filler_t **readpage)
{
  *readpage = shmem_file_setup();
  return 0;
}

void i915_gem_alloc_object(filler_t **readpage)
{
  if (drm_gem_object_init(readpage) != 0)
    ;
}

void do_read_cache_page(filler_t *filler)
{
  filler(0);
}

void read_cache_page_gfp(filler_t **readpage)
{
 filler_t *filler = *readpage;
 do_read_cache_page(filler);
}

int i915_gem_object_get_pages_gtt(filler_t **readpage)
{
  read_cache_page_gfp(readpage);
  return 0;
}

int i915_gem_object_bind_to_gtt(filler_t **readpage)
{
  return i915_gem_object_get_pages_gtt(readpage);
}

int i915_gem_object_pin(filler_t **readpage)
{
  return i915_gem_object_bind_to_gtt(readpage);
}

int intel_init_ring_buffer(filler_t **readpage)
{
  i915_gem_alloc_object(readpage);
  return i915_gem_object_pin(readpage);
}

int intel_init_render_ring_buffer(filler_t **readpage)
{
  return intel_init_ring_buffer(readpage);
}

int i915_gem_init_ringbuffer(filler_t **readpage)
{
  return intel_init_render_ring_buffer(readpage);
}

int i915_load_modeset_init(filler_t **readpage)
{
  return i915_gem_init_ringbuffer(readpage);
}

int i915_driver_load()
{
  filler_t *readpage;
  return i915_load_modeset_init(&readpage);
}

int main()
{

  i915_driver_load();

  return 0;
}
