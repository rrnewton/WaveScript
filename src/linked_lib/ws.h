/*

  This file is used by both wsmlton and wsc2.  It *should* be used by
  all backends.  It defines types and helper functions that pertain to
  the C representation of WS values as exposed through the FFI.  

 */

#ifndef WSHEADER
#define WSHEADER

//################################################################################//
//                 Common Type Defs to wsc2 and wsmlton                           //
//################################################################################//

//#define ws_unit_t char
//#define ws_char_t char
//#define ws_bool_t char
//#define uint8_t unsigned char

typedef char ws_char_t;
typedef char ws_bool_t;
typedef char ws_unit_t;
typedef char ws_timebase_t;

//typedef unsigned char      uint8_t;
//typedef unsigned short int uint16_t;
//typedef unsigned int16_t uint16_t;

// A WS string is a null terminated char*:
// It may or may not have a length field and/or reference count
// metadata to the left of the pointer depending on GC config.
#define ws_string_t ws_char_t*


//################################################################################//
//                 Matters of memory layout and GC (wsc2)                         //
//################################################################################//

#ifdef WSC2

//typedef unsigned int refcount_t;
// 64 bit is giving me annoying alignment problems.  Let's make this a whole word:
typedef unsigned long refcount_t;

// Arrays are the same size as RCs for now (see below):
#define ARRLENSIZE sizeof(refcount_t) 
#define PTRSIZE sizeof(void*)

#include <locale.h>
char* commaprint(unsigned long long n);

// ============================================================
// Names like BASEMALLOC abstract the allocater API.
#ifdef USE_BOEHM
  #include <gc/gc.h>
  #define BASEMALLOC GC_MALLOC
//  #define BASEMALLOC GC_MALLOC_IGNORE_OFF_PAGE
  #define BASEFREE   GC_FREE
  #define BASEMALLOC_ATOMIC GC_MALLOC_ATOMIC
//  #define BASEMALLOC_ATOMIC GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE
  inline void* BASECALLOC(size_t count, size_t size) {
    size_t bytes = count*size;
    void* ptr = GC_MALLOC(bytes);
    bzero(ptr, bytes);
    return ptr;
  }
  inline void* BASECALLOC_ATOMIC(size_t count, size_t size) {
    size_t bytes = count*size;
    void* ptr = BASEMALLOC_ATOMIC(bytes);
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
  #define RCSIZE sizeof(refcount_t)
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

// ============================================================
// Handle RCs on Arrays (and maybe the same for cons cells):
// CLEAR_ARR_RC is only called when the ptr is non-nil.
#define ARR_RC_DEREF(ptr)         (((refcount_t*)ptr)[-1])
#define GET_ARR_RC(ptr)           (ptr ? ARR_RC_DEREF(ptr) : 0)
#define SET_ARR_RC(ptr,val)       (ARR_RC_DEREF(ptr) = val)
#define CLEAR_ARR_RC(ptr)         SET_ARR_RC(ptr,0)

// TEMPORARY: For the moment we make all incr/decrs thread safe!!
// Instead we should be proactively copying on fan-out.
#ifdef WS_THREADED
  #include "atomic_incr_intel.h"
  #define INCR_ARR_RC(ptr)        if (ptr) atomic_increment( &ARR_RC_DEREF(ptr))
  // Assumes that atomic_exchange_and_add returns the OLD value.
  #define DECR_ARR_RC_PRED(ptr)   (ptr ? atomic_exchange_and_add( &ARR_RC_DEREF(ptr), -1) == 1 : 0)
  #define INCR_ITERATE_DEPTH()    atomic_increment(&iterate_depth)
  #define DECR_ITERATE_DEPTH()    atomic_exchange_and_add(&iterate_depth, -1)
#else
  #define INCR_ARR_RC(ptr)        if (ptr) ARR_RC_DEREF(ptr)++
  #define DECR_ARR_RC_PRED(ptr)   (ptr ? (-- ARR_RC_DEREF(ptr) == 0) : 0)

//int foo2(int a, int b) {   return b; }
// #define DECR_RC_PRED(ptr) foo2(ptr ? printf("   Decr %p rc %d\n", ptr, GET_RC(ptr)) : 99, (ptr ? (--(((refcount_t*)ptr)[-1]) == 0) : 0))

  #define INCR_ITERATE_DEPTH()  iterate_depth++
  #define DECR_ITERATE_DEPTH()  iterate_depth--
#endif

//#define DECR_RC_PRED(ptr) (ptr ? (GET_RC(ptr) <=0 ? printf("ERROR: DECR BELOW ZERO\n") : (--(((refcount_t*)ptr)[-1]) == 0)) : 0)

// Handle Cons Cell memory layout:
// ------------------------------------------------------------
// Normally, the refcount routines are the same:
#define CLEAR_CONS_RC(ptr)    SET_CONS_RC(ptr,0)

#define GET_CONS_RC        GET_ARR_RC
#define SET_CONS_RC        SET_ARR_RC
#define INCR_CONS_RC       INCR_ARR_RC
#define DECR_CONS_RC_PRED  DECR_ARR_RC_PRED

// Here I'm testing different layouts:
// ------------------------------------------------------------
// Cell consists of [cdr] [RC] ->[car]
#define CONSCELL(ty)       (void*)((char*)WSMALLOC(PTRSIZE+RCSIZE + sizeof(ty)) + PTRSIZE+RCSIZE);
#define CAR(ptr,ty)        (*ptr)
#define SETCAR(ptr,hd,ty)  (CAR(ptr,ty) = hd)
#define CONSPTR(ptr)       ((void**)((char*)ptr - (PTRSIZE+RCSIZE)))
#define CDR(ptr,ty)        (*CONSPTR(ptr))
#define SETCDR(ptr,tl,ty)  (*CONSPTR(ptr)=tl)
#define FREECONS(ptr,ty)   WSFREE(CONSPTR(ptr))

// New layout to ameliorate alignment problems for the CDR field on 64 bit,  [RC], ->[CDR], [CAR]
// ------------------------------------------------------------
// (NOTE: Relative to the start of the malloc its still unaligned, but should make valgrind happy.)
// NOPE: it doesn't make valgrind happy.
/* #define CONSCELL(ty)      (void*)((char*)WSMALLOC(RCSIZE + PTRSIZE + sizeof(ty)) + RCSIZE); */
/* #define CAR(ptr,ty)       (*((ty*)(((void**)ptr)+1))) */
/* #define SETCAR(ptr,hd,ty) (CAR(ptr,ty) = hd) */
/* #define CDR(ptr,ty)       (*(void**)ptr) */
/* #define SETCDR(ptr,tl,ty) (CDR(ptr) = tl) */
/* #define FREECONS(ptr,ty)  WSFREE(((char*)ptr) - RCSIZE) */

// Now we'll try this: [CDR], [CAR], [RC] ->
// ------------------------------------------------------------
// WEIRD: still a valgrind problem, even though both cdr and car should be aligned relative to the malloc.
// But they are NOT aligned relative to the pointer.  Apparently valgrind needs both.
/* #define CONSCELL(ty)      (void*)((char*)WSMALLOC(RCSIZE + PTRSIZE + sizeof(ty)) + RCSIZE+PTRSIZE + sizeof(ty)); */
/* #define CAR(ptr,ty)       (*((ty*)(((char*)ptr) - RCSIZE - sizeof(ty)))) */
/* #define SETCAR(ptr,hd,ty) (CAR(ptr,ty) = hd) */
/* #define CDR(ptr,ty)       (*((void**)(((char*)ptr) - RCSIZE - PTRSIZE - sizeof(ty)))) */
/* #define SETCDR(ptr,tl,ty) (CDR(ptr,ty) = tl) */
/* #define FREECONS(ptr,ty)  WSFREE(((char*)ptr) - RCSIZE - PTRSIZE - sizeof(ty)) */

// Finally [CDR], [CAR], -> [RC]
// ------------------------------------------------------------


// How bout with a struct?
// ------------------------------------------------------------
// This makes valgrind happy.  Won't work with the ZCT though, unless we do the same trick for arrays.
/* struct cons_cell { */
/*   refcount_t rc; */
/*   void* cdr; */
/*   int car; // This is a lie. */
/* }; */
/* #define CONSCELL(ty)      ((struct cons_cell*)(WSMALLOC(sizeof(struct cons_cell) + sizeof(ty) - sizeof(int)))) */
/* #define CAR(ptr,ty)       (*(ty*)(&(((struct cons_cell*)ptr)->car))) */
/* #define SETCAR(ptr,hd,ty) (CAR(ptr,ty) = hd) */
/* #define CDR(ptr,ty)       (((struct cons_cell*)ptr)->cdr) */
/* #define SETCDR(ptr,tl,ty) (CDR(ptr) = tl) */
/* #define FREECONS(ptr,ty)  WSFREE(((char*)ptr) - RCSIZE - PTRSIZE - sizeof(ty)) */

/* #define CONS_RC(ptr)            (((struct cons_cell*)ptr)->rc) */
/* #define GET_CONS_RC(ptr)        (ptr ? CONS_RC(ptr) : 0) */
/* #define SET_CONS_RC(ptr,val)    (CONS_RC(ptr) = val) */
/* #define INCR_CONS_RC(ptr)       if (ptr) CONS_RC(ptr)++ */
/* #define DECR_CONS_RC_PRED(ptr)  (ptr ? ( -- CONS_RC(ptr) == 0) : 0) */


// Handle Array memory layout:
// ------------------------------------------------------------
// An array consists of [len] [RC] [elem*]
// Both len and RC are currently the same type (refcount_t)
// this makes the access uniform.
#define ARRLEN(ptr)        (ptr ? ((refcount_t*)ptr)[ARRLENOFFSET] : 0)
//#define ARRLEN(ptr)        ((int*)ptr)[-2]
// This should not be used on a null pointer:
#define SETARRLEN(ptr,len) (((refcount_t*)ptr)[ARRLENOFFSET]=len)

#define ARRLEN(ptr)        (ptr ? ((refcount_t*)ptr)[ARRLENOFFSET] : 0)
// Get a pointer to the *start* of the thing (the pointer to free)
// The length field is assumed to be the leftmost field:
#define ARRPTR(ptr)        (((refcount_t*)ptr) + ARRLENOFFSET)
#define FREEARR(ptr)       WSFREE(ARRPTR(ptr))

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



// This is not currently used by the code generator [2008.07.02], but can be used by C code.
// HOWEVER: It does not employ the _SCALAR allocation routines.
//
//#define WSARRAYALLOC(len,ty) ((void*)((char*)calloc(ARRLENSIZE+RCSIZE + (len * sizeof(ty)), 1) + ARRLENSIZE+RCSIZE))
#define WSARRAYALLOC(len,ty) (ws_array_alloc(len, sizeof(ty)))
#define WSSTRINGALLOC(len)   (ws_array_alloc(len, sizeof(ws_char_t)))

// NOTE: This currently should work with boehm, but will waste memory
// by allocating space for a refcount even though it isn't used.
inline void* ws_array_alloc(int len, int eltsize) {
  char* ptr = ((char*)WSCALLOC(ARRLENSIZE + RCSIZE + len*eltsize, 1)) + ARRLENSIZE+RCSIZE;
  //char* ptr = ((char*)WSMALLOC(ARRLENSIZE + RCSIZE + len*eltsize)) + ARRLENSIZE+RCSIZE;
  SETARRLEN(ptr, len);
#ifndef USE_BOEHM
  CLEAR_ARR_RC(ptr);
#endif
  return ptr;
}

// Nasty duplication:
inline void* ws_array_alloc_scalar(int len, int eltsize) {
  char* ptr = ((char*)WSCALLOC_SCALAR(ARRLENSIZE + RCSIZE + len*eltsize, 1)) + ARRLENSIZE+RCSIZE;
  //char* ptr = ((char*)WSMALLOC_SCALAR(ARRLENSIZE + RCSIZE + len*eltsize)) + ARRLENSIZE+RCSIZE;
  SETARRLEN(ptr, len);
#ifndef USE_BOEHM
  CLEAR_ARR_RC(ptr);
#endif
  return ptr;
}


// I can't see any portable way but to expose macros for each scalar type.
// TODO: FINISH THIS LIST:
#define WSARRAYALLOC_CHAR(len)   WSARRAYALLOC(len, ws_char_t)
#define WSARRAYALLOC_INT(len)    WSARRAYALLOC(len, int)
#define WSARRAYALLOC_FLOAT(len)  WSARRAYALLOC(len, float)
#define WSARRAYALLOC_DOUBLE(len) WSARRAYALLOC(len, double)


#define WSARRAYSET(arr,i,v)  arr[i] = v
#define WSARRAYGET(arr,i)    arr[i]


#endif // End WSC2 

//################################################################################//

#ifdef WSMLTON

// Note, MLton's foreign.sml defines all of the WSARRAYALLOC_* entrypoints above.

#endif // End WSMLTON 

//################################################################################//

// Shared bits used in multiple backends:

#define moduloI(a,b) (a % b)

#endif // End header
