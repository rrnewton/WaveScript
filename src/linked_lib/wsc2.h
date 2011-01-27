/*

 [2007.12.06]

    This is the header that goes with my new C backend.  It contains
    various macros that keep bloat from the emit-c2 pass itself.
 
   -Ryan

 Important preprocessor variables:
   LOAD_COMPLEX -- we need complex support, bring in complex.h
   USE_BOEHM    -- a conservative garbage collector
   ALLOC_STATS  -- print allocation statics at various points
   WS_THRADED   -- turn on threading

 */

// For some godawful reason sched.h won't work out of the box and I need to do this:
#define _GNU_SOURCE 
// And it has to be done *before* stdio.h is included...

// Headers that we need for the generated code:
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include <unistd.h>
#include<math.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include<getopt.h>

// For gettid()
//#include <sys/types.h>
//#include<syscall.h>

#define LOAD_COMPLEX
//#define WS_THREADED
//#define ALLOC_STATS
//#define BLAST_PRINT

#define TRUE  1
#define FALSE 0

#ifdef WS_THREADED
// For now, only use real-time timers in threaded mode:
//#define WS_REAL_TIMERS
#include <pthread.h>
#endif

#ifdef LOAD_COMPLEX
#include<complex.h>
#endif

typedef unsigned char      uint8_t;
typedef unsigned short int uint16_t;

#include "ws.h"

extern int stopalltimers;

void wserror_fun(char*);
void wserror_builtin(char*);

// ============================================================
// ZCT handling for deferred reference counting:
// ============================================================

#ifdef WS_USE_ZCT

typedef unsigned char typetag_t;

// 80 (64+16) KB for now:
//#define ZCT_SIZE (1024*16)
// Testing: 16mb
//#define ZCT_SIZE (1024 * 1048 * 4)
#define ZCT_SIZE (1024 * 1048 * 16)

// These will need to be per-thread in the future:
typedef struct {
  int count;
  typetag_t tags[ZCT_SIZE];
  void* ptrs[ZCT_SIZE];
} zct_t;

/* extern typetag_t zct_tags[]; */
/* extern void*     zct_ptrs[]; */
/* extern int       zct_count; */
extern int       iterate_depth;

#ifdef WS_THREADED
  // We no longer share a ZCT, so it needn't  be locked.
  //extern pthread_mutex_t zct_lock;
  extern zct_t** all_zcts;
#else 
  extern zct_t* zct;
#endif

void free_by_numbers(typetag_t, void*);

//============================================================
// We mark everything that's added to the ZCT so that we don't
// double-add objects resulting in double-frees.

// This needs to be the high-bit in a refcount_t
#define PUSHED_MASK (((refcount_t)1) << (sizeof(refcount_t) * 8 - 1))

// NOTE: THIS ASSUMES IDENTICAL RC METHOD FOR ARRAYS AND CONS CELLS!!
static inline void MARK_AS_PUSHED(void* ptr) {
  //SET_RC(ptr, GET_RC(ptr) | PUSHED_MASK);
  ARR_RC_DEREF(ptr) |= PUSHED_MASK;
}
// NOTE: THIS ASSUMES IDENTICAL RC METHOD FOR ARRAYS AND CONS CELLS!!
static inline void UNMARK_AS_PUSHED(void* ptr) {
  //SET_RC(ptr, GET_RC(ptr) | PUSHED_MASK);
  ARR_RC_DEREF(ptr) &= ~PUSHED_MASK;
}
// NOTE: THIS ASSUMES IDENTICAL RC METHOD FOR ARRAYS AND CONS CELLS!!
static inline void PUSH_ZCT(zct_t* zct, typetag_t tag, void* ptr) {
  //printf("pushing %p tag %d, rc %u, (mask %u)\n", ptr, tag, GET_RC(ptr), PUSHED_MASK);
  // TEMPORARILY LOCKING FOR ACCESS TO CENTRALIZED ZCT:
#ifdef WS_THREADED
  //    pthread_mutex_lock(&zct_lock);
#endif 
  if (ptr == NULL) return;
  if (GET_ARR_RC(ptr) & PUSHED_MASK) { 
    //printf("ALREADY PUSHED %p, tag %d\n", ptr, tag);
    return; // Already pushed.
  }

  //#ifdef WSDEBUG
  if (zct->count == ZCT_SIZE) {
    wserror_builtin("ZCT overflow");
  }
  //#endif

  MARK_AS_PUSHED(ptr);
  zct->tags[zct->count] = tag;
  zct->ptrs[zct->count] = ptr;
  zct->count++;
#ifdef WS_THREADED
  //    pthread_mutex_unlock(&zct_lock);
#endif
}

#ifdef BLAST_PRINT
#define histo_len 20
unsigned long tag_histo[histo_len];
#endif

// The depth argument to BLAST_ZCT is PRIOR to decrement (iterate_depth--). 
// So we're looking for depth==1 not depth==0.
static inline void BLAST_ZCT(zct_t* zct, int depth) {
  int i;
  int freed = 0;
  int max_tag = 0;
 
// [2008.11.07] TEMP: in the process of working on a true disjoint-heaps implementation.
// In the mean time we let EVERYTHING leak from the ZCT version when threads are on.
#ifdef WS_THREADED
  zct->count = 0;
  return;
#endif

  if (depth > 1) {
    //printf("Not blasting, depth %d\n", iterate_depth);
    return;
  }

#ifdef WS_THREADED
  //    pthread_mutex_lock(&zct_lock);
#endif
#ifdef BLAST_PRINT
      if (zct->count==0) return; printf(" ** BLASTING:" ); fflush(stdout);
      for(i=0; i<histo_len; i++) tag_histo[i]=0;
#endif
  for(i=zct->count-1; i>=0; i--) {
    // Wipe off the mask bit before checking:
    if (0 == (GET_ARR_RC(zct->ptrs[i]) & ~PUSHED_MASK)) {
        #ifdef BLASTING 
          if (zct->tags[i] < histo_len) tag_histo[zct->tags[i]]++;
          max_tag = (max_tag > zct->tags[i]) ? max_tag : zct->tags[i];
        #endif
      free_by_numbers(zct->tags[i], zct->ptrs[i]);
      freed++;
    } else UNMARK_AS_PUSHED(zct->ptrs[i]);
  }  
#ifdef BLAST_PRINT
      printf(" killed %d/%d, tag histo: [ ", freed, zct->count); 
      for(i=0; (i<max_tag+1) && (i<histo_len); i++) printf("%d ", tag_histo[i]);
      printf("]\n");fflush(stdout);
      #ifdef ALLOC_STATS
        ws_alloc_stats();
      #endif
#endif
  zct->count = 0;
#ifdef WS_THREADED
  //    pthread_mutex_unlock(&zct_lock);
#endif
}

// Temporary: Currently we only enforce separate heaps in THREADED mode.
#ifdef WS_THREADED
#define FIFO_COPY_OUTGOING(name) fifo_copy_outgoing(& (name##_queue))
#else
#define FIFO_COPY_OUTGOING(name) {}
#endif



#endif // WS_USE_ZCT

// ============================================================
int outputcount = 0;
int wsc2_tuplimit = 10;

//################################################################################//
//                           Scheduler and data passing
//################################################################################//


// --------------------------------------------------------------------------------
#ifdef WS_REAL_TIMERS

unsigned long tick_counter;
//double last_time; // In milliseconds.
double start_of_time = 0;
double logical_time = 0;

unsigned long long total_events = 0;
// In the same units as clock()

/* [2008.09.09] If we try to usleep for too small an interval, it
 * won't work.  Thus we instead try to maintain an average timer rate.
 * If we are behind where we should be, we don't wait at all.  
 *
 * [2008.10.06] There's a question as to how long of a time horizon we
 * should have in doing this.  If we look at the average rate since
 * the start of time, then a particularly bad lag will skew our rate
 * for a long time.  Perhaps we want a shorter time horizon, or an
 * EWMA or something.
 */
inline void wait_ticks(double delta) { // Delta in milliseconds

  //double now = clock() * (1000000 / CLOCKS_PER_SEC); // microseconds
  struct timeval tmp;
  gettimeofday(&tmp, NULL);
  double now = tmp.tv_sec * 1000000 + tmp.tv_usec;
  //printf("  now %g  ", now);

  // TODO: Set this at the beginning of time from the init function:
  if (start_of_time == 0.0) start_of_time = now;

  logical_time += 1000 * delta * tick_counter; // milliseconds, no microseconds
  tick_counter = 0;
  
  double actual_time = now - start_of_time; // microseconds:

  // HACK: Only bother waiting if we owe more than 5ms:
  if (logical_time > actual_time )//+ 5000)
  {
    double diff = logical_time - actual_time;
    //printf("sleep %g, logical %g, actual %g\n", diff, logical_time, actual_time);
    usleep(diff);
    //nanosleep(diff);
    // HACK: because clock() only measures process time, not sleeping time, we need
    // to manually update the "actual_time" interval here!
        //start_of_time -= diff;    
  } else {
    //printf(".");
  }
}

#define VIRTTICK() tick_counter++
#define WAIT_TICKS(delta) wait_ticks(delta)

// --------------------------------------------------------------------------------
#else // Not WS_REAL_TIMERS:

#define VIRTTICK()                   {}
#define WAIT_TICKS(delta)            {}
#endif



// Single threaded version:
// ================================================================================
// For one thread, these macros do nothing.  We don't use realtime for timers.
#ifndef WS_THREADED
#ifdef WS_USE_ZCT
#define EMIT(val, ty, fn) fn(zct, val)
#else
#define EMIT(val, ty, fn) fn(val)
#endif

#define TOTAL_WORKERS(count)         {}
#define REGISTER_WORKER(ind, ty, fp) {}
#define DECLARE_WORKER(ind, ty, fp) 
#define START_WORKERS()              {}
unsigned long print_queue_status() { return 0; }

// There are (currently) no communication queues in the single threaded version.
#define GRAB_WRITEFIFO(name)    {}
#define RELEASE_WRITEFIFO(name) {}

#else

// Thread-per-operator version, various FIFO implementations:
// ================================================================================

//#include <pthread.h>

// Pick a FIFO implementation:
//============================================================
//#include "simple_wsfifo.c"
//#include "twostage_wsfifo.c"
//#include "c_fifos/bits/old.c"

#ifdef  WS_LOCK_FREE_FIFO
// Linked list fifos that are lock-free.
// TODO: Env Vars: FIFO_REUSE_NODES

// For now I'm hacking this to be blocking, which involves adding
// locks to a lock-free fifo implementation!
#include "midishare_fifo/wsfifo.c"

#elif WS_ARRAY_FIFO
// This one is a bounded fifo.
//#include "simple_bounded_wsfifo2.c"
#error "Bounded array based FIFOs are not finished yet."

//#elif WS_LIST_FIFO
#else 
// List based locking fifos.  Supports fine and course grained
// locking, single or two stage enqueuing.
// Env Vars: FIFO_TWOSTAGE  FIFO_COARSE_LOCKING
#include "c_fifos/bits/list_fifo.c"

#endif

//============================================================

#define FIFO_CONST_SIZE 100
#define ANY_CPU -1

#ifdef WS_USE_ZCT
void fifo_copy_outgoing(wsfifo* ff) {
  int i;
  int pending = wsfifo_pending(ff); 
  for(i=0; i < pending ; i++) { 
    void* ptr = wsfifo_recheck(ff);
    //printf("  Considering %p ... refcount \n", ptr);
    wsfifo_release_one(ff);
  }
}
#endif

void (**worker_table) (void*);   // Should we pad this to prevent false sharing?
wsfifo** queue_table;            // Should we pad this to prevent false sharing?
int* cpu_affinity_table;         // This should be set based on AST annotations... currently set to ANY_CPU
int total_workers;

#ifdef __linux__

//#include <sys/types.h>
// Not used yet, but this will set the thread/cpu affinity.
#include <syscall.h>
#include <sched.h>
void pin2cpu(int cpuId) {
      // Get the number of CPUs
   if (cpuId != ANY_CPU) {
      cpu_set_t mask;
      unsigned int len = sizeof(mask);
      CPU_ZERO(&mask);
      CPU_SET(cpuId, &mask);

      //printf("Process id %u, thread id %u\n", getpid(), pthread_self());
      //printf("The ID of this of this thread is: %ld\n", (long int)syscall(224));
      //printf("The ID of this of this thread is: %ld\n", (long int)syscall(__NR_gettid));
      //long int tid = syscall(224); // No idea how portable this is...

      long int tid = syscall(__NR_gettid);

      //int retval = sched_setaffinity(gettid(), len, &mask);
      //#define gettid() syscall(__NR_gettid)

      int retval = sched_setaffinity(tid, len, &mask);
      if (retval != 0) {
	perror("sched_setaffinity");
	exit(-1);
      }
   }
}
// This restricts the thread to N CPUS 
void pin2cpuRange(int numcpus) {
      cpu_set_t mask;
      int i;
      unsigned int len = sizeof(mask);
      CPU_ZERO(&mask);
      for (i=0; i<numcpus; i++)  CPU_SET(i, &mask);
      long int tid = syscall(SYS_gettid);

      printf("PINNING TO CPUS 0 ... %d, TID %d\n", numcpus-1, tid);
      int retval = sched_setaffinity(tid, len, &mask);
      if (retval != 0) {
	perror("sched_setaffinity");
	exit(-1);
      }
}

#else
  #error "pin2cpu: Cpu Pinning only works under linux presently."
#endif



pthread_mutex_t print_lock = PTHREAD_MUTEX_INITIALIZER;

// These are hooks that we can use to lock the fifo for the entire run of an operator... if we like.
// These pass through the name of the queue.
#define GRAB_WRITEFIFO(name)    grab_wsfifo((& name##_queue))
#define RELEASE_WRITEFIFO(name) release_wsfifo((& name##_queue))
//#define GRAB_READFIFO(name)    {}
//#define RELEASE_READFIFO(name) {}


// Declare the existence of each operator.
#ifdef WS_USE_ZCT
// Here we need to pass a ZCT also, we grab it from the global table.
#define DECLARE_WORKER(ind, ty, fp) wsfifo fp##_queue;  void fp##_wrapper(void* x) { fp(all_zcts[ind], *(ty*)x);  }
#else
#define DECLARE_WORKER(ind, ty, fp) wsfifo fp##_queue; void fp##_wrapper(void* x) { fp(*(ty*)x);  }
#endif

// Declare number of worker threads.
// This uses plain old malloc... tables are allocated once.

/* #define TOTAL_WORKERS(count) { \ */
/*    int i; \ */
/*    worker_table  = malloc(sizeof(void*) * count);  \ */
/*    queue_table   = malloc(sizeof(wsfifo*) * count);  \ */
/*    cpu_affinity_table = malloc(sizeof(int) * count);  \ */
/*    total_workers = count; \ */
/*    ALLOC_ZCT(count); \ */
/* } */

void TOTAL_WORKERS(int count) { 
   int i; 
   worker_table  = malloc(sizeof(void*) * count);  
   queue_table   = malloc(sizeof(wsfifo*) * count);  
   cpu_affinity_table = malloc(sizeof(int) * count);  
   total_workers = count; 
#ifdef WS_USE_ZCT
   all_zcts = malloc(count * sizeof(void*)); 
   for(i=0; i<count; i++) { 
     all_zcts[i] = malloc(sizeof(zct_t)); 
     all_zcts[i]->count = 0; 
  }
#endif
}

// Register a function pointer for each worker.
#define REGISTER_WORKER(ind, ty, fp) { \
   wsfifoinit(& fp##_queue, FIFO_CONST_SIZE, sizeof(ty));   \
   worker_table[ind] = & fp##_wrapper;  \
   queue_table[ind]  = & fp##_queue; \
   cpu_affinity_table[ind]  = ANY_CPU; \
}

// Enqueue a datum in another thread's queue.
#ifdef WS_USE_ZCT
//#define EMIT(val, ty, fn) WS_INTERNAL_QUEUE(& fn##_queue, val, ty);
//#define EMIT(val, ty, fn) WSFIFOPUT(& fn##_internal_queue, val, ty);
#define EMIT(val, ty, fn) WSFIFOPUT(& fn##_queue, val, ty);
#else
#define EMIT(val, ty, fn) WSFIFOPUT(& fn##_queue, val, ty);
#endif

// This defines the main loop for each WS thread.  Grab messages and process them.
void* worker_thread(void* i) {
  int index = (int)(size_t)i;
  pthread_mutex_lock(&print_lock);
  if (cpu_affinity_table[index] == ANY_CPU) fprintf(stderr, "** Spawning worker thread %d\n", index);
  else fprintf(stderr, "** Spawning worker thread %d, cpu %d\n", index, cpu_affinity_table[index]);

  // In this mode we restrict the subset of CPUs that we use.
#ifdef __linux__
#ifdef LIMITCPUS
  // For now this mode cannot restrict which cpu *within* the subset each thread uses.
  char* str = getenv("LIMITCPUS");
  if (str == 0) str =  "1";
  int cpus = atoi(str);
  //printf("  LIMITING TO %d CPUS (from %s)\n", cpus, str);  fflush(stdout);
  pin2cpuRange(cpus);
#else
  pin2cpu(cpu_affinity_table[index]);
#endif
#else
  #error "worker_thread: Cpu Pinning only works under linux presently."
#endif

  pthread_mutex_unlock(&print_lock);  

  // This loop is very simple because there's CURRENTLY only one input queue for the thread:
  // We don't need a "select":
  while (1) 
  {
#ifdef FIFO_COARSE_LOCKING
    // [2008.11.04] We need to grab/release for reading also..
    // But, once we grab, we should dequeue until its empty before releasing...
    int i;
    grab_wsfifo(queue_table[index]);
    wsfifo_wait(queue_table[index]);
    int size = wsfifosize(queue_table[index]);

/* if (size>0) printf("Grabbing fifo %p to empty it... elements %d\n" , queue_table[index], size); */
/*     if (queue_table[index]->buffer.head != NULL || queue_table[index]->buffer.tail != NULL)  */
/*       { fflush(stdout); fflush(stderr); */
/*         wserror_builtin("Grabbed for reading: THE FIRST STAGE BUFFER SHOULD BE NULL"); } */
    for(i=0; i<size; i++) {
      //printf("Reading out element %d\n", i);
#endif

    // Accesses to these two tables are read-only:
    void* ptr = wsfifoget(queue_table[index]);
    (*worker_table[index])(ptr);
    wsfifoget_cleanup(queue_table[index]);

 #ifdef FIFO_COARSE_LOCKING
    }
    release_wsfifo(queue_table[index]);
 #endif
  }
  return 0;
}

// Start the scheduler.
void START_WORKERS() {
  int i;  
  for (i=0; i<total_workers; i++)  { 
    pthread_t threadID; 
    pthread_create(&threadID, NULL, &worker_thread, (void*)(size_t)i);	
  } 
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
    fprintf(stderr,"Stopped all timers.\n");
  #ifdef WS_THREADED
    print_queue_status();
  #endif
}

void wsInternalInit() {
#ifdef USE_BOEHM
  GC_INIT();
  printf("GC INIT COMPLETE.\n");
#endif
}

// Note, in a threaded scenario, this is driven by a DECLARE_WORKER
// same as any other operator.  That's too bad.  It doesn't really
// deserve its own thread.
#ifdef WS_USE_ZCT
void BASE(zct_t* zct, char x) {
#else
void BASE(char x) {
#endif
  outputcount++;
  if (outputcount == wsc2_tuplimit) { 
    fprintf(stderr, "Enough tuples.  Shutting down.\n");
    wsShutdown(); 
    exit(0);     
  }
#ifdef ALLOC_STATS
  ws_alloc_stats();
#endif
  // To flush or not to flush?
  // This is an annoying issue.  In certain circumstances (like when I
  // was trying to hook up client/server partitions via stderr/stdin),
  // it very much helps to flush on output.  But on the other hand,
  // several benchmarks produce huge quantities of output tuples, and
  // it's slow to flush on each one.
#ifdef WS_FLUSH_ON_EMIT
  fflush(stdout); 
#endif
}

// This is a WS array of strings containing all the command line
// arguments to the program at runtime.  We will need to be very
// careful what we do with this in distributed settings.  Either it
// only makes sense on the machine where the program was spawned, or
// we need to transmit the data to other machines.
ws_string_t* ws_command_line_arguments = (ws_string_t*)0;
ws_string_t* ws_get_command_line() {
  return ws_command_line_arguments;
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
  };
  if (optind < argc) {
    int i;
    // Here we allocate a WS ARRAY and copy the strings over to WS strings
    ws_command_line_arguments = WSARRAYALLOC(argc-optind, ws_string_t);
    for(i=0; i< argc-optind; i++) {
      ws_string_t wsstr = WSSTRINGALLOC(strlen(argv[i+optind]) + 1);
      // Here we depend on the fact that WS strings look like C strings to the right of the ptr:
      strcpy(wsstr, argv[i+optind]);
      WSARRAYSET(ws_command_line_arguments, i, wsstr);
    }
  }
}

// FIXME: When driven by foreign source we don't use this:
void wserror_fun(char* msg) {
  //error(msg);
  printf("Failed with error: %s\n", msg);
  exit(-1);
}
void wserror_builtin(char* str) { wserror_fun(str); }

//#define wserror_builtin(str) wserror_fun(str)
// FIXME: TODO DISABLE THIS #def WHEN WE'RE DRIVEN BY A FOREIGN SOURCE:
// Or, remove the ability of foreign sources to define their own wserror
// Actually... that needs to be part of how foreign sources compose.
//#define wserror(str) wserror_fun(str)

int wsexit_fun(int code) {
  exit(code);
  return 0;
}

//################################################################################//
//                                    Misc                                        //
//################################################################################//

#define WSNULLTIMEBASE ((char)0)
#define TIMEBASE(n)    ((char)n)

// I don't have foreign *variables* yet, only foreign functions.  So we use these wrappers:
FILE* ws_get_stdout() { return stdout; }
FILE* ws_get_stderr() { return stderr; }
FILE* ws_get_stdin () { return stdin ; }

static inline char ws_lshiftC(char c, int n) { return c << n; }
static inline char ws_lorC  (char c, char d) { return c | d; }

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

// This just prints a number comma delimited for readability.
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
      else  comma = ',';
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
inline static float cNorm(complex float c) {
   float re =  __real__ (c);
   float im =  __imag__ (c);
   return sqrt ((re*re) + (im*im));
}
#endif

