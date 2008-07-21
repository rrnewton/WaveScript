

//   UNFINISHED!!!!!!!!!!

//################################################################################//
//                     Simple bounded fifo using mutexes.
//################################################################################//

#include <pthread.h>

typedef struct wsfifo {
  pthread_mutex_t mut;
  pthread_condition_t ready;
  pthread_condition_t room;
  void* buffer;
  int start;
  int end;
} wsfifo;

void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize) {
  pthread_mutex_init(& ff->mut,   NULL); 
  pthread_cond_init (& ff->ready, NULL); 
  pthread_cond_init (& ff->room,  NULL); 
  ff->buffer = malloc(elemsize * optional_size_limit);
  ff->start = 0;
  ff->end = 0;
}

#define WSFIFOPUT(ff, val, ty) {  \  
  pthread_mutex_lock  (& (ff)->mut);
  while (1) {
    if (FULL) // ((ff)->count == 0) 
    {
      
    }
  }
  ((ty*)ff->buffer)[i] = val;
  pthread_mutex_unlock(& (ff)->mut);
}

// We dequeue in two stages.  First we get the element, and then we
// "cleanup" to free the element we got.
void* wsfifoget(wsfifo* ff) {
}

// This doesn't check to make sure that last_cell is set!  It should!
#define wsfifoget_cleanup(ff) {}

/*
(define enqueue!
  (lambda (bq item)
    (let ([mutex (bq-mutex bq)])
      (with-mutex mutex
        (let loop ()
          (when (zero? (bq-i bq))
            (condition-wait (bq-room bq) mutex)
           ; we reacquire the mutex when we wake up, but some other
           ; thread may beat us to the punch
            (loop)))
        (let ([i (- (bq-i bq) 1)])
          (vector-set! (bq-vec bq) i item)
          (set-bq-i! bq i)
          (condition-signal (bq-ready bq)))))))

(define dequeue!
  (lambda (bq)
    (let ([mutex (bq-mutex bq)])
      (with-mutex mutex
        (let loop ()
          (when (= (bq-i bq) (vector-length (bq-vec bq)))
            (condition-wait (bq-ready bq) mutex)
           ; we reacquire the mutex when we wake up, but some other
           ; thread may beat us to the punch
            (loop)))
        (let ([i (bq-i bq)])
          (let ([item (vector-ref (bq-vec bq) i)])
            (set-bq-i! bq (+ i 1))
            (condition-signal (bq-room bq))
            item))))))
*/
