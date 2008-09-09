/*

 [2007.12.06]

    This is the header that goes with my new C backend.  It contains
    various macros that keep bloat from the emit-c2 pass itself.
 
   -Ryan

 Important preprocessor variables:
   LOAD_COMPLEX
   USE_BOEHM
   ALLOC_STATS 
   WS_THRADED

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

// For now, only use real-time timers in threaded mode:
#ifdef WS_THREADED
#define WS_REAL_TIMERS
#endif


#ifdef LOAD_COMPLEX
#include<complex.h>
#endif

#define TRUE  1
#define FALSE 0

#include "ws.h"

extern int stopalltimers;

void wserror_fun(char*);

// ZCT handling for deferred reference counting:
// ============================================================

//#ifdef USE_ZCT

typedef unsigned char typetag_t;

// 80 (64+16) KB for now:
//#define ZCT_SIZE (1024*16)
// Testing: 16mb
//#define ZCT_SIZE (1024 * 1048 * 4)
#define ZCT_SIZE (1024 * 1048 * 16)

// These will need to be per-thread in the future:
extern typetag_t zct_tags[];
extern void*     zct_ptrs[];
extern int       zct_count;
extern int       iterate_depth;

#ifdef WS_THREADED
// This locks all the zct_* above:
extern pthread_mutex_t zct_lock;
#endif

void free_by_numbers(typetag_t, void*);

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
static inline void PUSH_ZCT(typetag_t tag, void* ptr) {  
  //printf("pushing %p tag %d, rc %u, (mask %u)\n", ptr, tag, GET_RC(ptr), PUSHED_MASK);
  // TEMPORARILY LOCKING FOR ACCESS TO CENTRALIZED ZCT:
#ifdef WS_THREADED
    pthread_mutex_lock(&zct_lock);
#endif 
  if (ptr == NULL) return;
  if (GET_ARR_RC(ptr) & PUSHED_MASK) { 
    //printf("ALREADY PUSHED %p, tag %d\n", ptr, tag);
    return; // Already pushed.
  }

  //#ifdef WSDEBUG
  if (zct_count == ZCT_SIZE) {
    wserror_fun("ZCT overflow");
  }
  //#endif

  MARK_AS_PUSHED(ptr);
  zct_tags[zct_count] = tag;
  zct_ptrs[zct_count] = ptr;
  zct_count++;
#ifdef WS_THREADED
    pthread_mutex_unlock(&zct_lock);
#endif
}

#ifdef BLAST_PRINT
#define histo_len 20
unsigned long tag_histo[histo_len];
#endif

// The depth argument to BLAST_ZCT is PRIOR to decrement (iterate_depth--). 
// So we're looking for depth==1 not depth==0.
static inline void BLAST_ZCT(int depth) {
  int i;
  int freed = 0;
  int max_tag = 0;
  if (depth > 1) {
    //printf("Not blasting, depth %d\n", iterate_depth);
    return;
  }

#ifdef WS_THREADED
    pthread_mutex_lock(&zct_lock);
#endif
#ifdef BLAST_PRINT
      if (zct_count==0) return; printf(" ** BLASTING:" ); fflush(stdout);
      for(i=0; i<histo_len; i++) tag_histo[i]=0;
#endif
  for(i=zct_count-1; i>=0; i--) {
    // Wipe off the mask bit before checking:
    if (0 == (GET_ARR_RC(zct_ptrs[i]) & ~PUSHED_MASK)) {
        #ifdef BLASTING 
          if (zct_tags[i] < histo_len) tag_histo[zct_tags[i]]++;
          max_tag = (max_tag > zct_tags[i]) ? max_tag : zct_tags[i];
        #endif
      free_by_numbers(zct_tags[i], zct_ptrs[i]);
      freed++;
    } else UNMARK_AS_PUSHED(zct_ptrs[i]);
  }  
#ifdef BLAST_PRINT
      printf(" killed %d/%d, tag histo: [ ", freed, zct_count); 
      for(i=0; (i<max_tag+1) && (i<histo_len); i++) printf("%d ", tag_histo[i]);
      printf("]\n");fflush(stdout);
      #ifdef ALLOC_STATS
        ws_alloc_stats();
      #endif
#endif
  zct_count = 0;
#ifdef WS_THREADED
    pthread_mutex_unlock(&zct_lock);
#endif
}

//#endif // USE_ZCT

// ============================================================
int outputcount = 0;
int wsc2_tuplimit = 10;

#define moduloI(a,b) (a % b)

//################################################################################//
//                           Scheduler and data passing
//################################################################################//

#ifdef WS_REAL_TIMERS
unsigned long tick_counter;
#define VIRTTICK() tick_counter++
// Should use nanosleep:
#define WAIT_TICKS(delta) { \
  usleep(1000 * delta * tick_counter); \
  tick_counter = 0; }
#else
#define VIRTTICK()                   {}
#define WAIT_TICKS(delta)            {}
#endif


// Single threaded version:
// ============================================================
// For one thread, these macros do nothing.  We don't use realtime for timers.
#ifndef WS_THREADED
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

#ifdef USE_BOEHM
//#include <gc/gc_pthread_redirects.h>
#include <pthread.h>
#else
#include <pthread.h>
#endif

//#include "midishare_fifo/wsfifo.c"
#include "simple_wsfifo.c"
//#include "simple_bounded_wsfifo2.c"
#define FIFO_CONST_SIZE 100
#define ANY_CPU -1

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
   cpu_affinity_table = malloc(sizeof(int) * count);  \
   total_workers = count; \
}
// Register a function pointer for each worker.
#define REGISTER_WORKER(ind, ty, fp) { \
   wsfifoinit(& fp##_queue, FIFO_CONST_SIZE, sizeof(ty));   \
   worker_table[ind] = & fp##_wrapper;  \
   queue_table[ind]  = & fp##_queue; \
   cpu_affinity_table[ind]  = ANY_CPU; \
}
// Start the scheduler.
#define START_WORKERS() {                    \
  int i;  \
  for (i=0; i<total_workers; i++)  { \
    pthread_t threadID; \
    pthread_create(&threadID, NULL, &worker_thread, (void*)(size_t)i);	\
  } \
}
// Enqueue a datum in another thread's queue.
#define EMIT(val, ty, fn) WSFIFOPUT(& fn##_queue, val, ty);

void* worker_thread(void* i) {
  int index = (int)(size_t)i;
  pthread_mutex_lock(&print_lock);
  if (cpu_affinity_table[index] == ANY_CPU)
    fprintf(stderr, "** Spawning worker thread %d\n", index);
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

void BASE(char x) {
  outputcount++;
  if (outputcount == wsc2_tuplimit) { 
    fprintf(stderr, "Enough tuples.  Shutting down.\n");
    wsShutdown(); 
    exit(0);     
  }
#ifdef ALLOC_STATS
  ws_alloc_stats();
#endif
  //fflush(stdout); // [2008.07.31] No more flushing at BASE for now.
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
#define wserror_wsc2(str) wserror_fun(str);

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
inline static float cNorm(complex float c) {
   float re =  __real__ (c);
   float im =  __imag__ (c);
   return sqrt ((re*re) + (im*im));
}
#endif

