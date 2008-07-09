

// [2007.12.06] This is the header that goes with my new C backend. -Ryan

// Headers that we need for the generated code:
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>

#define LOAD_COMPLEX

#ifdef LOAD_COMPLEX
#include<complex.h>
#endif

#include<getopt.h>

//int* arrayMake(size_t size, int len, ) { }

#define TRUE  1
#define FALSE 0

#define ws_unit_t char
#define ws_char_t char
#define ws_bool_t char
#define uint8_t unsigned char

#define ws_string_t char*

#define PTRSIZE sizeof(void*)
#define RCSIZE sizeof(int)
#define ARRLENSIZE sizeof(int)

// These macros allow us to monitor allocation rates if we wish:
//#define WSMALLOC malloc
//#define WSCALLOC calloc

//#ifdef ALLOC_STATS
long long alloc_counter = 0;
inline void* malloc_measured(size_t size) {
  alloc_counter += size;
  return malloc(size);
}
inline void* calloc_measured(size_t count, size_t size) {
  alloc_counter += size * count;
  return calloc(count,size);
}
#define WSMALLOC malloc_measured
#define WSCALLOC calloc_measured


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
#define FREECONS(ptr)  free((char*)ptr - sizeof(void*) - sizeof(int))

// This was from when RC's where the same size as a void* pointer:
//#define CDR(ptr)       (((void**)ptr)[-2])
//#define SETCDR(ptr,tl) (((void**)ptr)[-2])=tl

// Handle Array memory layout:
// An array consists of [len] [RC] [elem*]
// Both len and RC are currently ints:
#define ARRLEN(ptr)        (ptr ? ((int*)ptr)[-2] : 0)
//#define ARRLEN(ptr)        ((int*)ptr)[-2]
// This should not be used on a null pointer:
#define SETARRLEN(ptr,len) ((int*)ptr)[-2]=len

#define ARRLEN(ptr)        (ptr ? ((int*)ptr)[-2] : 0)
// Get a pointer to the *start* of the thing (the pointer to free)
#define ARRPTR(ptr)        (((void**)ptr)-2)
#define FREEARR(ptr)       free(ARRPTR(ptr))

// This is not currently used by the code generator [2008.07.02], but can be used by C code.
//#define WSARRAYALLOC(len,ty) ((void*)((char*)calloc(ARRLENSIZE+RCSIZE + (len * sizeof(ty)), 1) + ARRLENSIZE+RCSIZE))
#define WSARRAYALLOC(len,ty) (ws_array_alloc(len, sizeof(ty)))
#define WSSTRINGALLOC(len)   (ws_array_alloc(len, sizeof(ws_char_t)))

inline void* ws_array_alloc(int len, int eltsize) {
  char* ptr = ((char*)WSMALLOC(ARRLENSIZE + RCSIZE + len*eltsize)) + ARRLENSIZE+RCSIZE;
  SETARRLEN(ptr, len);
  CLEAR_RC(ptr);
  return ptr;
}


//typedef unsigned int16_t uint16_t;
typedef unsigned short int uint16_t;

#define moduloI(a,b) (a % b)

int outputcount = 0;
int wsc2_tuplimit = 10;

long long last_alloc_printed = 0;
void ws_alloc_stats() {
  printf("  Total allocation: %e\n", (double) alloc_counter);
  printf("  Alloctaion since last stats: %e\n", (double) alloc_counter-last_alloc_printed);
  last_alloc_printed = alloc_counter;
}
void wsShutdown() {
//#ifdef ???
  ws_alloc_stats();
}

void BASE(char x) { 
  outputcount++;
  if (outputcount == wsc2_tuplimit) { wsShutdown(); exit(0); }
//#ifdef ???
  ws_alloc_stats();
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



#ifdef LOAD_COMPLEX
inline static float cNorm(complex c) {
   float re =  __real__ (c);
   float im =  __imag__ (c);
   return sqrt ((re*re) + (im*im));
}
#endif

