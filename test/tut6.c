
#include <pthread.h>
#include <ciltut.h>

int counter = 0;
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

int main()
{
  toggle_lock_tracking();
  pthread_mutex_lock(&mtx);
  counter++;
  pthread_mutex_unlock(&mtx);
  return 0;
}
