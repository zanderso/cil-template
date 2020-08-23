 
#define _GNU_SOURCE   
#include <stdint.h>   
#include <pthread.h>  
#include <dlfcn.h>    
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ciltut.h>

struct cache_stack_entry {
  uint64_t start_miss;
  uint64_t start_refs;
  uint64_t start_time;
};

#define MAX_CACHE_STACK_ENTRIES 256
struct cache_stack {
  struct cache_stack_entry s[MAX_CACHE_STACK_ENTRIES];
  int t;
};

pthread_key_t CS_key;
static void init_CS()
{
  struct cache_stack *CS = calloc(1, sizeof(*CS));
  pthread_setspecific(CS_key, CS);
}

CONSTRUCTOR static void init_CS_key()
{
  pthread_key_create(&CS_key, &free);
  init_CS();
}

static struct cache_stack *get_CS()
{
  return (struct cache_stack *)pthread_getspecific(CS_key);
}

int (*pthread_create_orig)(pthread_t *__restrict,
                           __const pthread_attr_t *__restrict,
                           void *(*)(void *),
                           void *__restrict) = NULL;

extern void *checked_dlsym(void *handle, const char *sym);

CONSTRUCTOR static void init_cache_stack()
{
  pthread_create_orig = checked_dlsym(RTLD_NEXT, "pthread_create");
}

struct pthread_closure {
  void *(*fn)(void *);
  void *arg;
};

static void *tfunc_wrapper(void *arg)
{
  struct pthread_closure *c = (struct pthread_closure *)arg;
  void *(*fn)(void *) = c->fn;
  void *a             = c->arg;
  void *res           = NULL;

  free(c);

  init_CS();
  perf_init(gettid());
  res = fn(a);
  perf_deinit();

  return res;
}

int pthread_create(pthread_t *__restrict thread,
                   __const pthread_attr_t *__restrict attr,
                   void * (*start_routine)(void *),
                   void *__restrict arg)
{
  struct pthread_closure *c = malloc(sizeof(struct pthread_closure));
  int res;

  c->fn  = start_routine;
  c->arg = arg;

  res = pthread_create_orig(thread, attr, &tfunc_wrapper, c);
  if (res != 0) {
    printf("pthread failed\n");
    fflush(stdout);
    free(c);
  }

  return res;
}

void tut_cache_begin(char const *f, int l)
{
  struct cache_stack *cs = get_CS();
  
  cs->s[cs->t].start_miss = perf_get_cache_miss();
  cs->s[cs->t].start_refs = perf_get_cache_refs();
  cs->s[cs->t].start_time = tut_get_time();
  cs->t++;
  
  return;
}

void tut_cache_end(char const *f, int l)
{
  uint64_t final_miss, final_refs, final_time;
  uint64_t net_miss,   net_refs,   net_time;
  double miss_rate;
  double bandwidth;
  struct cache_stack *cs = get_CS();

  final_miss = perf_get_cache_miss();
  final_refs = perf_get_cache_refs();
  final_time = tut_get_time();

  net_miss = final_miss - cs->s[cs->t - 1].start_miss;
  net_refs = final_refs - cs->s[cs->t - 1].start_refs;
  net_time = final_time - cs->s[cs->t - 1].start_time;

  miss_rate = (double)net_miss/(double)net_refs;
  bandwidth = (double)(net_miss * 1000000000ULL)/(double)net_time;
  printf("%s:%d Miss rate was: %f, Bandwidth was %f\n",
         f, l, miss_rate, bandwidth);
  fflush(stdout);

  cs->t--;
  return;
}
