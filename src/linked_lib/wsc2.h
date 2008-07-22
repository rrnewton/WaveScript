
/*

 [2007.12.06]

    This is the header that goes with my new C backend.  It contains
    various macros that keep bloat from the emit-c2 pass itself.
 
   -Ryan

 Important preprocessor variables:
   LOAD_COMPLEX
   USE_BOEHM
   ALLOC_STATS 
   WS_THREADED

 */

// Headers that we need for the generated code:
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include<getopt.h>

#define LOAD_COMPLEX
//#define WS_THREADED
//#define ALLOC_STATS

#ifdef LOAD_COMPLEX
#include<complex.h>
#endif

#define TRUE  1
#define FALSE 0

//#define ws_unit_t char
//#define ws_char_t char
//#define ws_bool_t char
//#define uint8_t unsigned char

typedef char ws_char_t;
typedef char ws_bool_t;
typedef char ws_unit_t;

typedef unsigned char      uint8_t;
typedef unsigned short int uint16_t;
//typedef unsigned int16_t uint16_t;

#define ws_string_t char*

extern int stopalltimers;

//################################################################################//
//                 Matters of memory layout and GC                                //
//################################################################################//

#define PTRSIZE sizeof(void*)
#define ARRLENSIZE sizeof(int)

#include <locale.h>
char* commaprint(unsigned long long n);

// ============================================================
// Names like BASEMALLOC abstract the allocater API.
#ifdef USE_BOEHM
  #include <gc/gc.h>
  #define BASEMALLOC GC_MALLOC
  #define BASEFREE   free
  #define BASEMALLOC_ATOMIC GC_MALLOC_ATOMIC
  inline void* BASECALLOC(size_t count, size_t size) {
    size_t bytes = count*size;
    void* ptr = GC_MALLOC(bytes);
    bzero(ptr, bytes);
    return ptr;
  }
  inline void* BASECALLOC_ATOMIC(size_t count, size_t size) {
    size_t bytes = count*size;
    void* ptr = GC_MALLOC_ATOMIC(bytes);
    bzero(ptr, bytes);
    return ptr;
  }
  // If we are using BOEHM we do not need refcounts:
  #define RCSIZE 0
  #define ARRLENOFFSET -1
#else
  #define BASEMALLOC        malloc
  #define BASECALLOC        calloc
  #define BASEMALLOC_ATOMIC malloc
  #define BASECALLOC_ATOMIC calloc
  #define BASEFREE   free
  #define RCSIZE sizeof(int)
  #define ARRLENOFFSET -2
#endif

// ============================================================
// Then on top of that "BASE" layer we define the "WS" variants.
// These macros allow us to monitor allocation rates if we wish:
#ifdef ALLOC_STATS
unsigned long long alloc_total = 0;
unsigned long long alloc_counter = 0;
unsigned long long free_counter = 0;
inline void* malloc_measured(size_t size) {
  alloc_total   += size;
  alloc_counter += 1;
  return BASEMALLOC(size);
}
inline void* calloc_measured(size_t count, size_t size) {
  alloc_total += size * count;
  alloc_counter += 1;
  return BASECALLOC(count,size);
}
inline void free_measured(void* object) {
  free_counter += 1;
  BASEFREE(object);
}
#define WSMALLOC        malloc_measured
#define WSMALLOC_SCALAR malloc_measured
#define WSCALLOC        calloc_measured
#define WSCALLOC_SCALAR calloc_measured
#define WSFREE   free_measured
#else
#define WSMALLOC        BASEMALLOC
#define WSCALLOC        BASECALLOC
#define WSMALLOC_SCALAR BASEMALLOC_ATOMIC
#define WSCALLOC_SCALAR BASECALLOC_ATOMIC
#define WSFREE   BASEFREE
#endif

// Handle RCs on BOTH Cons Cells and Arrays:
// A RC is the size of an int currently:
#define CLEAR_RC(ptr)                ((int*)ptr)[-1] = 0
#define INCR_RC(ptr)        if (ptr) ((int*)ptr)[-1]++
#define DECR_RC_PRED(ptr) (ptr && --(((int*)ptr)[-1]) == 0)
#define GET_RC(ptr)                  ((int*)ptr)[-1]

// Handle Cons Cell memory layout:
// Cell consists of [cdr] [RC] [car]
#define CONSCELL(ty)   (void*)((char*)WSMALLOC(PTRSIZE+RCSIZE + sizeof(ty)) + PTRSIZE+RCSIZE);
#define CAR(ptr)       (*ptr)
#define CDR(ptr)       (*(void**)(((char*)ptr) - (PTRSIZE+RCSIZE)))
#define SETCDR(ptr,tl) (((void**)(((char*)ptr) - (PTRSIZE+RCSIZE)))[0])=tl
#define SETCAR(ptr,hd) ptr[0]=hd
#define FREECONS(ptr)  WSFREE((char*)ptr - sizeof(void*) - sizeof(int))

// This was from when RC's where the same size as a void* pointer:
//#define CDR(ptr)       (((void**)ptr)[-2])
//#define SETCDR(ptr,tl) (((void**)ptr)[-2])=tl

// Handle Array memory layout:
// An array consists of [len] [RC] [elem*]
// Both len and RC are currently ints:
#define ARRLEN(ptr)        (ptr ? ((int*)ptr)[ARRLENOFFSET] : 0)
//#define ARRLEN(ptr)        ((int*)ptr)[-2]
// This should not be used on a null pointer:
#define SETARRLEN(ptr,len) ((int*)ptr)[ARRLENOFFSET]=len

#define ARRLEN(ptr)        (ptr ? ((int*)ptr)[ARRLENOFFSET] : 0)
// Get a pointer to the *start* of the thing (the pointer to free)
#define ARRPTR(ptr)        (((void**)ptr) + ARRLENOFFSET)
#define FREEARR(ptr)       WSFREE(ARRPTR(ptr))

// This is not currently used by the code generator [2008.07.02], but can be used by C code.
//#define WSARRAYALLOC(len,ty) ((void*)((char*)calloc(ARRLENSIZE+RCSIZE + (len * sizeof(ty)), 1) + ARRLENSIZE+RCSIZE))
#define WSARRAYALLOC(len,ty) (ws_array_alloc(len, sizeof(ty)))
#define WSSTRINGALLOC(len)   (ws_array_alloc(len, sizeof(ws_char_t)))

inline void* ws_array_alloc(int len, int eltsize) {
  char* ptr = ((char*)WSMALLOC(ARRLENSIZE + RCSIZE + len*eltsize)) + ARRLENSIZE+RCSIZE;
  SETARRLEN(ptr, len);
#ifndef USE_BOEHM
  CLEAR_RC(ptr);
#endif
  return ptr;
}

#define moduloI(a,b) (a % b)

int outputcount = 0;
int wsc2_tuplimit = 10;

#ifdef ALLOC_STATS
unsigned long long last_alloc_printed = 0;
void ws_alloc_stats() {
  printf("  Malloc calls: %s\n", commaprint(alloc_counter));
  printf("  Free   calls: %s", commaprint(free_counter));
  printf("\t\t Unfreed objs: %s\n", commaprint(alloc_counter-free_counter));
  printf("  Total bytes allocated: %s\n",  commaprint(alloc_total));
  printf("  Bytes since last stats: %s\n", commaprint(alloc_total - last_alloc_printed));
  last_alloc_printed = alloc_total;
}
#endif

//################################################################################//
//                           Scheduler and data passing
//################################################################################//

// Single threaded version:
// ============================================================
// For one thread, these macros do nothing.  We don't use realtime for timers.
#ifndef WS_THREADED
#define VIRTTICK()                   {}
#define WAIT_TICKS(delta)            {}
#define EMIT(val, ty, fn) fn(val)
#define TOTAL_WORKERS(count)         {}
#define REGISTER_WORKER(ind, ty, fp) {}
#define DECLARE_WORKER(ind, ty, fp) 
#define START_WORKERS()              {}
unsigned long print_queue_status() { return 0; }

#else

// Thread-per-operator version, midishare FIFO implementation:
// ============================================================

// For now I'm hacking this to be blocking, which involves adding
// locks to a lock-free fifo implementation!

#include <unistd.h>
unsigned long tick_counter;
#define VIRTTICK() tick_counter++
// Should use nanosleep:
#define WAIT_TICKS(delta) { \
  usleep(1000 * delta * tick_counter); \
  tick_counter = 0; }

#ifdef USE_BOEHM
//#include <gc/gc_pthread_redirects.h>
#include <pthread.h>
#else
#include <pthread.h>
#endif

//#include "midishare_fifo/wsfifo.c"
#include "simple_wsfifo.c"
#define FIFO_CONST_SIZE 100

void (**worker_table) (void*);   // Should we pad this to prevent false sharing?
wsfifo** queue_table;            // Should we pad this to prevent false sharing?
pthread_cond_t** cond_table;
pthread_mutex_t** mut_table;
int total_workers;

pthread_mutex_t print_lock = PTHREAD_MUTEX_INITIALIZER;

// Declare the existence of each operator.
#define DECLARE_WORKER(ind, ty, fp) wsfifo fp##_queue;  \
  void fp##_wrapper(void* x) { \
    fp(*(ty*)x); \
  }
// Declare number of worker threads.
// This uses plain old malloc... tables are allocated once.
#define TOTAL_WORKERS(count) { \
   worker_table  = malloc(sizeof(void*) * count);  \
   queue_table   = malloc(sizeof(wsfifo*) * count);  \
   total_workers = count; \
}
// Register a function pointer for each worker.
#define REGISTER_WORKER(ind, ty, fp) { \
   wsfifoinit(& fp##_queue, FIFO_CONST_SIZE, sizeof(ty));   \
   worker_table[ind] = & fp##_wrapper;  \
   queue_table[ind]  = & fp##_queue; \
}
// Start the scheduler.
#define START_WORKERS() {                    \
  int i;  \
  for (i=0; i<total_workers; i++)  { \
    pthread_t threadID; \
    pthread_create(&threadID, NULL, &worker_thread, (void*)i); \
  } \
}
// Enqueue a datum in another thread's queue.
#define EMIT(val, ty, fn) WSFIFOPUT(& fn##_queue, val, ty);

void* worker_thread(void* i) {
  int index = (int)i;
  pthread_mutex_lock(&print_lock);
  fprintf(stderr, "** Spawning worker thread %d\n", index);
  pthread_mutex_unlock(&print_lock);
  while (1) 
  {
    // Accesses to these two tables are read-only:
    void* ptr = wsfifoget(queue_table[index]);
    (*worker_table[index])(ptr);
    wsfifoget_cleanup(queue_table[index]);
  }
  return 0;
}

// Returns the sum of the sizes of all queues.
unsigned long print_queue_status() {
  int i;
  unsigned long total = 0;
  printf("Status of %d queues:", total_workers);
  fflush(stdout);
  for(i=0; i<total_workers; i++) {
    //printf("Queue #%d: \t%d\n", i, wsfifosize(queue_table[i]));
    int size = wsfifosize(queue_table[i]);
    total += size;
    printf(" %d", size);
    fflush(stdout);
  }
  printf(" total %d\n", total);
  return total;
}
#endif

//################################################################################//
//               Startup, Shutdown, Errors, Final output values                   //
//################################################################################//

void wsShutdown() {
  #ifdef ALLOC_STATS
    ws_alloc_stats();
  #endif
    stopalltimers = 1;
    printf("Stopped all timers.\n");
  #ifdef WS_THREADED
    print_queue_status();
  #endif
}

void BASE(char x) {
  outputcount++;
  if (outputcount == wsc2_tuplimit) { 
    printf("Enough tuples.  Shutting down.\n");
    wsShutdown(); 
    exit(0);     
  }
#ifdef ALLOC_STATS
  ws_alloc_stats();
#endif
  fflush(stdout);
}

void ws_parse_options(int argc, char** argv) {
  int i, c;
  while ((c = getopt(argc, argv, "n:")) != -1) {
    //printf("Parsing option character: %c\n", c);
	switch (c) {
	case 'n':
	        wsc2_tuplimit = atoi(optarg);
		break;
	// case 's': // Do not print main stream output tuples.
	default:
	  //		usage();
	  //		return 1;
		break;
	}
  }
}

// FIXME: When driven by foreign source we don't use this:
void wserror_fun(char* msg) {
  //error(msg);
  printf("Failed with error: %s\n", msg);
  exit(-1);
}
#define wserror_wsc2(str) wserror_fun(str);

//################################################################################//
//                                    Misc                                        //
//################################################################################//


/*
// TODO:
int Listlength(void* list) {
  int acc = 0;
  printf("List len... %p\n", list);
  while (list != 0) {
    list = CDR(list);
    acc++;
  }
  return acc; 
}
*/

/*
// TODO:
void* Listappend(void* ls1, void* ls2) {
  printf("List append... %p and %p\n", ls1, ls2);
  return ls1;
}

// TODO:
void* Listreverse(void* ls) {
  printf("List reverse... %p\n", ls);
  return ls;
}
*/


// This won't work:
/*
int Listref(void* list, int n) {
  return 0; 
}
*/


char *commaprint(unsigned long long n)
{
	static int comma = '\0';
	static char retbuf[30];
	char *p = &retbuf[sizeof(retbuf)-1];
	int i = 0;

	if(comma == '\0') {
		struct lconv *lcp = localeconv();
		if(lcp != NULL) {
			if(lcp->thousands_sep != NULL &&
				*lcp->thousands_sep != '\0')
				comma = *lcp->thousands_sep;
			else	comma = ',';
		}
	}

	*p = '\0';

	do {
		if(i%3 == 0 && i != 0)
			*--p = comma;
		*--p = '0' + n % 10;
		n /= 10;
		i++;
	} while(n != 0);

	return p;
}


#ifdef LOAD_COMPLEX
inline static float cNorm(complex c) {
   float re =  __real__ (c);
   float im =  __imag__ (c);
   return sqrt ((re*re) + (im*im));
}
#endif

