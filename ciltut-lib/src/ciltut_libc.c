#ifndef __MACH__
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <sys/syscall.h>
#include <linux/perf_event.h> 

static __thread pid_t cached_tid = -1;
pid_t gettid()
{
	if (cached_tid == -1) {
		cached_tid = (pid_t) syscall (SYS_gettid);
	}
	return cached_tid;
}

void *checked_dlsym(void *handle, const char *sym)
{
    void *res = dlsym(handle,sym);
    if(res == NULL) {
        char *error = dlerror();
        if(error == NULL) {
            error = "checked_dlsym: sym is NULL";
        }
        fprintf(stderr, "checked_dlsym: %s\n", error);
        exit(-1);
    }
    return res;
}

#define CACHE_REFS_CTR    0
#define CACHE_MISS_CTR    1
static struct perf_event_attr peattrs[] = {
  {.type = PERF_TYPE_HARDWARE, .config = PERF_COUNT_HW_CACHE_REFERENCES},
  {.type = PERF_TYPE_HARDWARE, .config = PERF_COUNT_HW_CACHE_MISSES},
};

static __thread int perf_counter_fds[] = {-1, -1};

static inline int
sys_perf_event_open(struct perf_event_attr *attr, pid_t pid, int cpu,
                    int group_fd, unsigned long flags)
{
  attr->size = sizeof(*attr);
  return syscall(__NR_perf_event_open, attr, pid, cpu, group_fd, flags);
}

static void open_perf_counter(pid_t pid, int counter)
{
  if (perf_counter_fds[counter] < 0) {
    int fd = -1;
    struct perf_event_attr *attr = &peattrs[counter];

    attr->inherit = 1;
    fd = sys_perf_event_open(attr, pid, -1, -1, 0);
    if (fd < 0) {
      perror("sys_perf_event_open_failed");
    }

    perf_counter_fds[counter] = fd;
  }
  return;
}

static uint64_t read_perf_counter(int *perffds, int counter)
{
  uint64_t val = 0;
  size_t res;

  res = read(perffds[counter], &val, sizeof(uint64_t));
  if (res == -1) {
    perror("read");
  }

  return val;
}

static void close_perf_counter(int counter)
{
  if (perf_counter_fds[counter] > 0) {
    close(perf_counter_fds[counter]);
    perf_counter_fds[counter] = -1;
  }
  return;
}

void perf_init(pid_t pid)
{
 	int i;
  for (i = 0; i < sizeof(peattrs) / sizeof(peattrs[0]); i++) {
    open_perf_counter(pid, i);
  }
  return;
}

void perf_deinit()
{
  int i;
  for (i = 0; i < sizeof(peattrs) / sizeof(peattrs[0]); i++) {
    close_perf_counter(i);
  }
  return;
}

uint64_t perf_get_cache_refs()
{
  return read_perf_counter(&perf_counter_fds[0], CACHE_REFS_CTR);
}

uint64_t perf_get_cache_miss()
{
  return read_perf_counter(&perf_counter_fds[0], CACHE_MISS_CTR);
}

static inline uint64_t nsecs_of_timespec(struct timespec *ts)
{
  return (uint64_t)ts->tv_sec * 1000000000ULL + (uint64_t)ts->tv_nsec;
}

uint64_t tut_get_time()
{
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return nsecs_of_timespec(&t);
}

#else

#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/syscall.h>
#include <mach/mach.h>
#include <mach/mach_time.h>

pid_t gettid()
{
	return (pid_t) syscall (SYS_gettid);
}

void *checked_dlsym(void *handle, const char *sym)
{
    void *res = dlsym(handle,sym);
    if(res == NULL) {
        char *error = dlerror();
        if(error == NULL) {
            error = "checked_dlsym: sym is NULL";
        }
        fprintf(stderr, "checked_dlsym: %s\n", error);
        exit(-1);
    }
    return res;
}

void perf_init(pid_t pid) {}
void perf_deinit() {}

uint64_t perf_get_cache_refs()
{
  return (uint64_t)0;
}

uint64_t perf_get_cache_miss()
{
  return (uint64_t)0;
}

uint64_t tut_get_time()
{
  uint64_t t;
  static mach_timebase_info_data_t tinfo;

  t = mach_absolute_time();
  if (tinfo.denom == 0) {
    mach_timebase_info(&tinfo);
  }

  return t * tinfo.numer / tinfo.denom;
}
#endif
