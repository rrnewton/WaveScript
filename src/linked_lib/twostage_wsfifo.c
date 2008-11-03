
/*
 [2008.10.30] This enables a two-step fifo that has both an "enqueue"
 and later an "inspect & release" phase.

 The simplest way to do this is as two fifos.

 */


#include <pthread.h>
#include <stdlib.h>
#include "ws.h"

// #define BASEFREE free

#define FIFOMALLOC BASEMALLOC
#define FIFOFREE   BASEFREE

//namespace Foo { 
//#include "simple_wsfifo.c"
//}

// ACK: I've duplicated the below from simple_wsfifo.c just to do an alpha rename. 
// It would be nice to have done this with C++ actually...
//================================================================================

typedef struct wsfifocell {
  struct wsfifocell* link;	/* next cell in the list */
  void* other_data;
} wsfifocell;

typedef struct one_wsfifo {
  struct wsfifocell*  head;
  struct wsfifocell*  tail;  
  struct wsfifocell*  last_popped;
  pthread_mutex_t     mut;
  pthread_cond_t has_data;
} one_wsfifo;

void one_wsfifoinit(one_wsfifo* ff, int optional_size_limit, int elemsize) {
  pthread_mutex_init(& ff->mut,   NULL); 
  pthread_cond_init (& ff->has_data, NULL); 
  ff->head = NULL;
  ff->tail = NULL;
  ff->last_popped = NULL;
}

inline void one_wsfifoput_cell(one_wsfifo* ff, wsfifocell* cell) {
  pthread_mutex_lock  (& (ff)->mut); 
  if ((ff)->tail == NULL)  
    (ff)->head = (wsfifocell*)cell;	
  else  
    (ff)->tail->link = (wsfifocell*)cell;   
  (ff)->tail = (wsfifocell*)cell;	  
  pthread_cond_signal(&(ff)->has_data); 
  pthread_mutex_unlock(& (ff)->mut); 
}

// We copy val into a new cell for transport.
#define one_WSFIFOPUT(ff, val, ty) {  \
  void** cell = FIFOMALLOC(sizeof(wsfifocell*) + sizeof(ty)); \
  cell[0] = (void*)NULL; /* not necessarily required */ \
  *(ty*)(cell+1) = val;  /* copy it over */ \
  one_wsfifoput_cell(ff, cell); \
}

// We dequeue in two stages.  First we get the element, and then we
// "cleanup" to free the element we got.
void* one_wsfifoget(one_wsfifo* ff) {
  pthread_mutex_lock(& (ff)->mut);
  //printf("Waiting on fifo %p\n", ff);

  while ((ff)->head == NULL)
    pthread_cond_wait(&(ff)->has_data, &(ff)->mut);

  // Assumes only one consumer, no one else to beat us to the punch:
  wsfifocell* cell = ff->head;
  //printf("Fifo %p got... %p, containing %d\n", ff, cell, ((int**)cell)[1]);

  // If we take the last element, null both.
  if ((ff)->head == (ff)->tail) (ff)->tail = NULL; 

  ff->head         = ff->head->link;
  ff->last_popped  = cell;

  pthread_mutex_unlock(& (ff)->mut);  
  return (void**)cell+1;
}

// This doesn't check to make sure that last_cell is set!  It should!
void one_wsfifoget_cleanup(one_wsfifo* ff) {
  FIFOFREE(ff->last_popped);
}

unsigned long one_wsfifosize(one_wsfifo* ff) {
  //printf("SIZE OF %p... grabbing lock\n", ff);
  pthread_mutex_lock  (& (ff)->mut);
  int count = 0;
  wsfifocell* ptr = ff->head;
  while (ptr != NULL) {
    ptr = ptr->link;
    count++;
  }
  pthread_mutex_unlock(& (ff)->mut);
  return count;
}

//============================================================

typedef struct wsfifo {
  one_wsfifo buffered;
  one_wsfifo outgoing;
  wsfifocell* in_limbo;
  int pending;
} wsfifo;

void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize) {
  one_wsfifoinit(&ff->buffered, optional_size_limit, elemsize);
  one_wsfifoinit(&ff->outgoing, optional_size_limit, elemsize);
  ff->in_limbo = NULL;
  ff->pending = 0;
}

// This needs to be audited.  Is there any potential problem with the
// pending count being outside the mutex?   
//#define WSFIFOPUT(ff, val, ty) { one_WSFIFOPUT(& (ff)->buffered, val, ty); ff->pending++; }
#define WSFIFOPUT(ff, val, ty) {  }

void* wsfifoget(wsfifo* ff) { return one_wsfifoget(& ff->buffered); }

void wsfifoget_cleanup(wsfifo* ff) {
  one_wsfifoget_cleanup(& ff->outgoing);
}

/* The more complex protocol is tha after a put, you must then do a
  release.  You may optionally do a 'check' before the release to peak
  at what you are about to release.  You don't get to replace the data
  item, but you get a pointer to it so that it can be mutated.
*/
void* wsfifo_recheck(wsfifo* ff) {
  ff->in_limbo = one_wsfifoget(& ff->buffered);
  return ff->in_limbo->other_data;
}

// Returns true if there are pending elements waiting to be released.
int wsfifo_pending(wsfifo* ff) {
  return (ff->pending == 0);
}

int wsfifosize(wsfifo* ff) {
  return one_wsfifosize(&ff->buffered) 
       + one_wsfifosize(&ff->outgoing);
}

/* This lets go of an enqueued item.  Release should be called once
   for each time WSFIFOPUT is called.  It will currently block if
   release is called before put.
 */
void wsfifo_release(wsfifo* ff) {
  if (ff->in_limbo == NULL)
    ff->in_limbo = one_wsfifoget(& ff->buffered);
  // We don't clean up, instead we pass the cell along.
  //one_wsfifoget_cleanup(& ff->buffered);
  one_wsfifoput_cell(& ff->outgoing, ff->in_limbo);
  ff->in_limbo = NULL;
  ff->pending--;
}
