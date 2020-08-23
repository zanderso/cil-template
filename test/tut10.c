#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <ciltut.h>

#define DATA_SIZE 50000000
#define TIMES 5
#define STEP  1

struct array {
  uint64_t *ptr;
  int size;
};

void *thrfunc(void *a)
{
  struct array *arr = (struct array *)a;
  uint64_t *ptr = arr->ptr;
  int size = arr->size;
  int i,j;

  cache_report {
    for (j = 0; j < TIMES; j++) {
      ptr[0] = 0x50505050;
      for (i = 1; i < size; i+=STEP)
        ptr[i] *= ptr[i-1];
    }
  }

  return NULL;
}

int main()
{
  struct array *arrays;
  pthread_t *tids;
  uint64_t *data;
  int num_cores;
  int i;

  num_cores = sysconf(_SC_NPROCESSORS_ONLN);
  printf("num_cores = %d\n", num_cores);
  fflush(stdout);
  num_cores = num_cores == -1 ? 1 : num_cores;
  arrays = malloc(num_cores * sizeof(struct array));
  tids   = malloc(num_cores * sizeof(pthread_t));
  data   = malloc(sizeof(uint64_t) * DATA_SIZE);

  for (i = 0; i < num_cores; i++) {
    arrays[i].ptr  = &data[(DATA_SIZE/num_cores) * i];
    arrays[i].size = DATA_SIZE/num_cores;
    pthread_create(&tids[i],NULL,&thrfunc,&arrays[i]);
  }

  for (i = 0; i < num_cores; i++) {
    pthread_join(tids[i], NULL);
  }

  return 0;
}

