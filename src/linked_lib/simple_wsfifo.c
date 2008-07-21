

//################################################################################//
//                   Simple fifo using mutexes & linked lists.
//################################################################################//

/* This implementation allocates a cell for each enqueue, which puts
   unnecessary stress on the memory allocator.
*/

#include <pthread.h>

#define FIFOMALLOC BASEMALLOC
#define FIFOFREE   BASEFREE

typedef struct wsfifocell {
  struct wsfifocell* link;	/* next cell in the list */
  void* other_data;
} wsfifocell;

typedef struct wsfifo {
  struct wsfifocell*  head;
  struct wsfifocell*  tail;  
  struct wsfifocell*  last_popped;
  pthread_mutex_t     mut;
  pthread_cond_t has_data;
} wsfifo;

void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize) {
  pthread_mutex_init(& ff->mut,   NULL); 
  pthread_cond_init (& ff->has_data, NULL); 
  ff->head = NULL;
  ff->tail = NULL;
  ff->last_popped = NULL;
}

// We copy val into a new cell for transport.
#define WSFIFOPUT(ff, val, ty) {  \
  pthread_mutex_lock  (& (ff)->mut); \
  void** cell = FIFOMALLOC(sizeof(wsfifocell*) + sizeof(ty)); \
  cell[0] = (void*)NULL;   /* not necessarily required */ \
  *(ty*)(cell+1) = val; /* copy it over */ \
  if ((ff)->tail == NULL)  \
    (ff)->head = (wsfifocell*)cell;	\
  else  \
    (ff)->tail->link = (wsfifocell*)cell;   \
  (ff)->tail = (wsfifocell*)cell;	  \
  pthread_cond_signal(&(ff)->has_data); \
  pthread_mutex_unlock(& (ff)->mut); \
}
/*

 */

// We dequeue in two stages.  First we get the element, and then we
// "cleanup" to free the element we got.
void* wsfifoget(wsfifo* ff) {
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
void wsfifoget_cleanup(wsfifo* ff) {
  FIFOFREE(ff->last_popped);
}

unsigned long wsfifosize(wsfifo* ff) {
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
