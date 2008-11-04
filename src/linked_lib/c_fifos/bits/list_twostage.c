
/*
 ================
 List based FIFO:
 ================

 This linked-list based FIFO allocates a cell for each enqueue, and it is unbounded.

 It also supports a "two-stage" mode where each "enqueue" is followed
 by an "inspect & release" phase.  The simplest way to do this is as two fifos.

 Author: Ryan Newton [2008.10.30]

*/

#include <pthread.h>
#include <stdlib.h>
#include "ws.h"

// #define BASEFREE free

/* ================================================================== 
   Important environment variables:
   ================================================================== 
*/

#define FIFOMALLOC BASEMALLOC
#define FIFOFREE   BASEFREE

// This file can be used either for a "onestage" or "twostage" fifo. 
// #define FIFO_TWOSTAGE

/* Locking can happen on every put/get, or it can be done seperately
   for batch get/put. */
#define FIFO_LOCK_EVERY


/* ==================================================================
   Single Stage Implementation
   ==================================================================
*/

typedef struct wsfifocell {
  struct wsfifocell* link;	/* next cell in the list */
  void* other_data;
} wsfifocell;

typedef struct onestage_wsfifo {
  struct wsfifocell*  head;
  struct wsfifocell*  tail;  
  struct wsfifocell*  last_popped;
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_t     mut;
  pthread_cond_t has_data;
#endif
} onestage_wsfifo;

void onestage_wsfifoinit(onestage_wsfifo* ff, int optional_size_limit, int elemsize) {
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_init(& ff->mut,   NULL); 
  pthread_cond_init (& ff->has_data, NULL); 
#endif
  ff->head = NULL;
  ff->tail = NULL;
  ff->last_popped = NULL;
}

inline void onestage_wsfifoput_cell(onestage_wsfifo* ff, wsfifocell* cell) {
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_lock  (& (ff)->mut); 
#endif
  if ((ff)->tail == NULL)  
    (ff)->head = (wsfifocell*)cell;	
  else  
    (ff)->tail->link = (wsfifocell*)cell;   
  (ff)->tail = (wsfifocell*)cell;	  
#ifdef FIFO_LOCK_EVERY
  pthread_cond_signal(&(ff)->has_data); 
  pthread_mutex_unlock(& (ff)->mut); 
#endif
}

// We copy val into a new cell for transport.
#define onestage_WSFIFOPUT(ff, val, ty) {  \
  void** cell = FIFOMALLOC(sizeof(wsfifocell*) + sizeof(ty)); \
  cell[0] = (void*)NULL; /* not necessarily required */ \
  *(ty*)(cell+1) = val;  /* copy it over */ \
  onestage_wsfifoput_cell(ff, cell); \
}

// We dequeue in two stages.  First we get the element, and then we
// "cleanup" to free the element we got.
void* onestage_wsfifoget(onestage_wsfifo* ff) {
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_lock(& (ff)->mut);
#endif
  //printf("Waiting on fifo %p\n", ff);
  while ((ff)->head == NULL) {
#ifdef FIFO_LOCK_EVERY
    pthread_cond_wait(&(ff)->has_data, &(ff)->mut);
#else 
#error "onestage_wsfifoget: Fixme... need some substitute for waiting on the signal."
#endif
  }

  // Assumes only one consumer, no one else to beat us to the punch:
  wsfifocell* cell = ff->head;
  //printf("Fifo %p got... %p, containing %d\n", ff, cell, ((int**)cell)[1]);

  // If we take the last element, null both.
  if ((ff)->head == (ff)->tail) (ff)->tail = NULL; 

  ff->head         = ff->head->link;
  ff->last_popped  = cell;

#ifdef FIFO_LOCK_EVERY
  pthread_mutex_unlock(& (ff)->mut);  
#endif
  return (void**)cell+1;
}

// This doesn't check to make sure that last_cell is set!  It should!
void onestage_wsfifoget_cleanup(onestage_wsfifo* ff) {
  FIFOFREE(ff->last_popped);
}

unsigned long onestage_wsfifosize(onestage_wsfifo* ff) {
  //printf("SIZE OF %p... grabbing lock\n", ff);
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_lock  (& (ff)->mut);
#endif
  int count = 0;
  wsfifocell* ptr = ff->head;
  while (ptr != NULL) {
    ptr = ptr->link;
    count++;
  }
#ifdef FIFO_LOCK_EVERY
  pthread_mutex_unlock(& (ff)->mut);
#endif
  return count;
}

/* ==================================================================
   Exposed interface
   ==================================================================

 Which interface we expose depends on the environment variables.
 (Yes, it would be better to do this with OOP, but we're sticking with C.)

*/

#ifndef FIFO_TWOSTAGE
// The single-stage version simply needs to expose the above
// implementation under different names:

typedef onestage_wsfifo wsfifo 

inline void grab_wsfifo(wsfifo* ff)    {}
inline void release_wsfifo(wsfifo* ff) {}

#define wsfifoinit        onestage_wsfifoinit
#define WSFIFOPUT         onestage_WSFIFOPUT
#define wsfifoget         onestage_wsfifoget
#define wsfifoget_cleanup onestage_wsfifoget_cleanup
#define wsfifosize        onestage_wsfifosize

void* wsfifo_recheck(wsfifo* ff) {
  wserror("wsfifo_recheck called, but fifo not configured for twostage operation");
  return 0;
}
void wsfifo_release_one(wsfifo* ff) {
  wserror("wsfifo_release_one called, but fifo not configured for twostage operation");
}

#else 
// The two stage version builds on the one-stage:

typedef struct wsfifo {
  onestage_wsfifo buffered;
  onestage_wsfifo outgoing;
  wsfifocell* in_limbo;
  int pending;
} wsfifo;

void grab_wsfifo(wsfifo* ff) {
  //pthread_mutex_lock(ff->mut)
}
void release_wsfifo(wsfifo* ff) {
  //pthread_mutex_unlock(ff->mut);
}

void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize) {
  onestage_wsfifoinit(&ff->buffered, optional_size_limit, elemsize);
  onestage_wsfifoinit(&ff->outgoing, optional_size_limit, elemsize);
  ff->in_limbo = NULL;
  ff->pending = 0;
}

// This needs to be audited.  Is there any potential problem with the
// pending count being outside the mutex?   
#define WSFIFOPUT(ff, val, ty) { onestage_WSFIFOPUT(& (ff)->buffered, val, ty); ff->pending++; }
//#define WSFIFOPUT(ff, val, ty) {  }

void* wsfifoget(wsfifo* ff) { return onestage_wsfifoget(& ff->buffered); }

void wsfifoget_cleanup(wsfifo* ff) {
  onestage_wsfifoget_cleanup(& ff->outgoing);
}

/* The more complex protocol is tha after a put, you must then do a
  release.  You may optionally do a 'check' before the release to peak
  at what you are about to release.  You don't get to replace the data
  item, but you get a pointer to it so that it can be mutated.
*/
void* wsfifo_recheck(wsfifo* ff) {
  ff->in_limbo = onestage_wsfifoget(& ff->buffered);
  return ff->in_limbo->other_data;
}

// Returns true if there are pending elements waiting to be released.
int wsfifo_pending(wsfifo* ff) {
  return (ff->pending == 0);
}

int wsfifosize(wsfifo* ff) {
  return onestage_wsfifosize(&ff->buffered) 
       + onestage_wsfifosize(&ff->outgoing);
}

/* This lets go of an enqueued item.  Release should be called once
   for each time WSFIFOPUT is called.  It will currently block if
   release is called before put.
 */
void wsfifo_release_one(wsfifo* ff) {
  if (ff->in_limbo == NULL)
    ff->in_limbo = onestage_wsfifoget(& ff->buffered);
  // We don't clean up, instead we pass the cell along.
  //onestage_wsfifoget_cleanup(& ff->buffered);
  onestage_wsfifoput_cell(& ff->outgoing, ff->in_limbo);
  ff->in_limbo = NULL;
  ff->pending--;
}
#endif
