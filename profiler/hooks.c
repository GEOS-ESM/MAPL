/* Prototypes for __malloc_hook, __free_hook */
#include <malloc.h>
     
/* Prototypes for our hooks.  */
void my_init_hook (void);
void my_restore_hook (void);


//void fortran_malloc_(void **, size_t *);
//void fortran_free_(void **);

void c_malloc_table(void *, size_t);
void c_free_table(void *);
size_t returnTotalMem();


static void *my_malloc_hook (size_t, const void *);
static void my_free_hook (void*, const void *);

static void **old_malloc_hook;
static void **old_free_hook;

/* Override initializing hook from the C library. */
//void (*__malloc_initialize_hook) (void) = my_init_hook;

 void
my_init_hook (void)
{
  old_malloc_hook = __malloc_hook;
  old_free_hook = __free_hook;
  __malloc_hook = my_malloc_hook;
  __free_hook = my_free_hook;
}

 void
my_restore_hook (void)
{
  /* Restore all old hooks */
  __malloc_hook = old_malloc_hook;
  __free_hook = old_free_hook;

  //  c_destroy_alloc_table();
}

void *
my_malloc_hook (size_t size, const void *caller)
{
  void *result;

  /* Restore all old hooks */
  __malloc_hook = old_malloc_hook;
  __free_hook = old_free_hook;
  /* Call recursively */
  result = malloc (size);

  /* printf might call malloc, so protect it too. */
  //  printf ("malloc (%u) returns %p\n", (unsigned int) size, result);
  //fortran_malloc_(&result, &size);
  c_malloc_table(result, size);
  
/* Save underlying hooks */
  old_malloc_hook = __malloc_hook;
  old_free_hook = __free_hook;
  /* Restore our own hooks */
  __malloc_hook = my_malloc_hook;
  __free_hook = my_free_hook;
  return result;
}

void
my_free_hook (void *ptr, const void *caller)
{
  /* Restore all old hooks */
  __malloc_hook = old_malloc_hook;
  __free_hook = old_free_hook;
  /* Call recursively */
  if (ptr != NULL) {
    free (ptr);
    /* printf might call free, so protect it too. */
    //  printf ("freed pointer %p\n", ptr);
    //fortran_free_(&ptr);
    c_free_table(ptr);
  }
  /* Save underlying hooks */
  old_malloc_hook = __malloc_hook;
  old_free_hook = __free_hook;
  /* Restore our own hooks */
  __malloc_hook = my_malloc_hook;
  __free_hook = my_free_hook;
}
 
void getMallocStat( size_t *tm, size_t *cnt) {
  //  *tm = totalMem;
  //  *cnt = totalCount;
  *tm = returnTotalMem();
}

