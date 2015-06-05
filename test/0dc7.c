
extern void *malloc (unsigned long __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


static const char *linked = (void*)0;

int sysfs_link_sibling(const char *s_name)
{
  if (linked != (void*)0)
    return (!strcmp(s_name,linked)) ? -17 : -12;

  linked = s_name;
  return 0;
}

void sysfs_unlink_sibling(const char *s_name)
{
  if (linked != (void*)0 && !strcmp(s_name,linked))
    linked = (void*)0;
}

int sysfs_create_dir(const char *name)
{
  return sysfs_link_sibling(name);
}

int kobject_add(const char *name)
{
  return sysfs_create_dir(name);
}

int *kobject_create_and_add(const char *name)
{
  int *kobj =(int*) malloc(sizeof(int));
  if (!kobj)
    return (void*)0;

  int retval = kobject_add(name);
  if (retval)
  {
    free(kobj);
    kobj = (void*)0;
  }

  return kobj;
}

int *class_compat_register(const char *name)
{
  int *kobj;

  kobj = kobject_create_and_add(name);
  if (!kobj)
    return (void*)0;

  return kobj;
}

void class_compat_unregister(const char *name)
{
  free(name);
}




static int *switch_class;


int create_extcon_class(void)
{

    switch_class = class_compat_register("switch");
    if (!switch_class)
      return -12;

  return 0;
}

int extcon_class_init(void)
{
  return create_extcon_class();
}

void extcon_class_exit(void)
{
  return;
}


int main(void)
{

  extcon_class_init();
  extcon_class_exit();
  extcon_class_init();

  return 0;
}
