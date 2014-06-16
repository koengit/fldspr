#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/*

README

First, generate a file called

  pthreads_include.c
  
using the function "showpThread" from Program.hs. You get an example by running

  Fldspr.main
  
Then, compile this file by running

  gcc -O2 -pthread -std=gnu99 pthreads.c
  
on the command line. Finally, execute the program by running

  ./a.out

on the command line.

*/

#define NUM_THREADS 4

/* --------------------------------------------------------------------------
-- synchronization
*/

pthread_barrier_t sync_barr;

void sync_init( int numThreads )
{
  pthread_barrier_init(&sync_barr, NULL, numThreads );
}

void sync()
{
  int ret = pthread_barrier_wait(&sync_barr);
  if(ret != 0 && ret != PTHREAD_BARRIER_SERIAL_THREAD)
  {
    printf("FATAL: Could not wait on barrier\n");
    exit(-1);
  }
}

/* -------------------------------------------------------------------------- */

#define ARR_SIZE 11000000
int arr[ARR_SIZE];
int arr_size = ARR_SIZE; 
int result[ARR_SIZE];

int f(int x) 
{
  int i;
  for ( i = 0; i < 10000; i++ )
    x = 1-x;
  return x;
}

#define min(x,y) ((x) > (y) ? (y) : (x))
#define max(x,y) ((x) > (y) ? (x) : (y))

typedef struct {
  int tid;
  int numThreads;
  int *mem;
} arg_struct;

#include "pthreads_include.c"
 
/* -------------------------------------------------------------------------- */

int main (int argc, char *argv[])
{
  int t;
  /* data init */
  for (t=0; t<ARR_SIZE; t++)
    arr[t] = t;

  /* start threads */
  cykel_start( NUM_THREADS );

  /* print results */
  /* for (t=0; t < ARR_SIZE; t++)
    printf("%d ", result[t]);
  printf("\n"); */
  
  /* finishing */
  pthread_exit(NULL);
}

/* -------------------------------------------------------------------------- */

