

//   UNFINISHED!!!!!!!!!!

//################################################################################//
//        Simple bounded fifo using mutexes, multi-reader multi-writer
//################################################################################//


/*
 * macros for the wrap bit in circular_queue counters
 */
#define FLIP_WRAP_32(x) ((x) ^ 0x80000000)
#define FLIP_WRAP_64(x) ((x) ^ 0x8000000000000000)
#define GET_WRAP_32(x) ((x) & 0x80000000)
#define GET_WRAP_64(x) ((x) & 0x8000000000000000)
#define HAVE_SAME_WRAP_32(x, y) !(GET_WRAP_32(x) ^ GET_WRAP_32(y))
#define HAVE_SAME_WRAP_64(x, y) !(GET_WRAP_64(x) ^ GET_WRAP_64(y))
#define MASK_WRAP_32(x) ((x) & 0x7FFFFFFF)
#define MASK_WRAP_64(x) ((x) & 0x7FFFFFFFFFFFFFFF)




#include <pthread.h>

typedef struct wsfifo {
   //pthread_mutex_t mut;
   //pthread_condition_t ready;
   //pthread_condition_t room;
  void* buffer;
  int buffer_size;
  int start;
  int end;
} wsfifo;

static int wsfifo_is_empty(wsfifo *ff)
{
  return
     ((MASK_WRAP_32(ff->start) == ((MASK_WRAP_32(ff->end)+1) ))
      && HAVE_SAME_WRAP_32(ff->start, ff->end))
     || ((MASK_WRAP_32(ff->start) == 0)
         && (MASK_WRAP_32(ff->end) == (ff->buffer_size-1))
         && !HAVE_SAME_WRAP_32(ff->start, ff->end));  
}

static int wsfifo_is_full(wsfifo *ff)
{
  return
     ((MASK_WRAP_32(ff->start) == ((MASK_WRAP_32(ff->end)+1) ))
      && !HAVE_SAME_WRAP_32(ff->start, ff->end))
     || ((MASK_WRAP_32(ff->start) == 0)
         && (MASK_WRAP_32(ff->end) == (ff->buffer_size-1))
         && HAVE_SAME_WRAP_32(ff->start, ff->end));
}

// assumes elemsize > 0
void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize)
{
   //pthread_mutex_init(& ff->mut,   NULL); 
   //pthread_cond_init (& ff->ready, NULL); 
   //pthread_cond_init (& ff->room,  NULL); 
  ff->buffer = malloc(elemsize * optional_size_limit);
  ff->buffer_size = elemsize;
  
  ff->end = 0;
  ff->start = 1;

  if (elemsize == 1) {
     ff->start = FLIP_WRAP_32(0);
  }
}


#define WSFIFOPUT(ff, val, ty) {                                                   \
  unsigned int tmp_end;                                                            \
  while (wsfifo_is_full(ff))                                                       \
  {printf("f\n");                                                       \
  }                                                                                \
  ((ty*)((ff)->buffer))[(MASK_WRAP_32((ff)->end)+1) % (ff)->buffer_size] = val;    \
  tmp_end =                                                                        \
     ((MASK_WRAP_32((ff)->end) + 1) % (ff)->buffer_size)                           \
     | GET_WRAP_32((ff)->end);                                                     \
  (ff)->end =                                                                      \
     (MASK_WRAP_32(tmp_end) == 0)                                                  \
     ? FLIP_WRAP_32(tmp_end)                                                       \
     : tmp_end;                                                                    \
}






void* wsfifoget(wsfifo* ff)
{
  // just spins
  while (wsfifo_is_empty(ff))
  {printf("e\n");
  }

  return ff->buffer + (MASK_WRAP_32(ff->start) % ff->buffer_size);
}

void wsfifoget_cleanup(wsfifo* ff)
{
  unsigned int tmp_start;

  tmp_start =
     ((MASK_WRAP_32(ff->start) + 1) % ff->buffer_size)
     | GET_WRAP_32(ff->start);
  ff->start =
     (MASK_WRAP_32(tmp_start) == 0)
     ? FLIP_WRAP_32(tmp_start)
     : tmp_start;  
}

// FIXME: this is not 100% accurate
unsigned long wsfifosize(wsfifo *ff)
{
   int tmp_start, tmp_end;

   tmp_start = ff->start;
   tmp_end = ff->end;

   if (HAVE_SAME_WRAP_32(tmp_start, tmp_end)) {
      return MASK_WRAP_32(tmp_end) - MASK_WRAP_32(tmp_start) + 1;
   } else {
      return MASK_WRAP_32(tmp_end) + ff->buffer_size - MASK_WRAP_32(tmp_start)+1;
   }
}


