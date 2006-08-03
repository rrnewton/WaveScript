
#include <complex.h>
#include <fftw3.h>
#include <time.h>

#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>

// [2006.08.03] This method seems to pass the obvious memory leak tests that I can think of.

// IS THIS LIBRARY THREAD SAFE??

typedef struct Plan_t {
  fftw_plan plan;
  fftw_complex* vec;
  int vec_len;
} plan_t;

int make_fftw_plan_dft_1d (int N) {
  // Heap allocate a struct.
  plan_t* p = malloc(sizeof(plan_t));
  p->vec_len = N;
  p->vec = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  if (N > 10000)
    p->plan = fftw_plan_dft_1d(N, p->vec, p->vec, FFTW_FORWARD, FFTW_ESTIMATE);
  else if (N > 5000)
    p->plan = fftw_plan_dft_1d(N, p->vec, p->vec, FFTW_FORWARD, FFTW_MEASURE);
  else 
    p->plan = fftw_plan_dft_1d(N, p->vec, p->vec, FFTW_FORWARD, FFTW_PATIENT);
  return (int)p;
}

void free_fftw_plan_dft_1d (int ptr) {
  plan_t* p = (plan_t*)ptr;
  fftw_destroy_plan(p->plan);
  fftw_free(p->vec);
  p->vec_len = 0;
  p->vec = 0;
  p->plan = 0;
  printf(".");
}


// This returns #t if successful, or a number (the correct length) if there was a mismatched length.
ptr s_fftw_execute (ptr vec, int plan) {
  int i;
  int len = Svector_length(vec);
  int N = len / 2;  
  clock_t start, end;
  plan_t* p = (plan_t*) plan;

  /*printf("Executing!! len %i  incoming len %i\n", p->vec_len, Svector_length(vec));
  for (i=0; i<10; i++)
    printf("  Got element %i %lf\n", i, ((double*)p->vec)[i]);
  printf("  Got element %i %lf\n", 262000, ((double*)p->vec)[262000]);
  printf("  Got element %i %lf\n", 524000, ((double*)p->vec)[524000]);*/

  // TODO: CHECK THAT LENGTH IS RIGHT!
  if (N != p->vec_len) {
    printf("Mismatched lengths! %i %i\n", N, p->vec_len);
    return(Sfixnum(p->vec_len));
  }

  //printf("Measuring... ");  fflush( 0 );
  //start = clock();
  //end = clock();
  //printf("Done. (time used %i)\n", end - start);  fflush( 0 );

  //printf("Filling... \n");  fflush( 0 );
  for(i=0; i<len; i+=2) {
    /*printf("Loading: real %lf, imag %lf\n",
	   Sflonum_value(Svector_ref(vec, i)),
	   Sflonum_value(Svector_ref(vec, i+1)));*/
    ((double*)p->vec)[i]   = Sflonum_value(Svector_ref(vec, i));
    ((double*)p->vec)[i+1] = Sflonum_value(Svector_ref(vec, i+1));
  }
  //printf("Done\n");
 
  //printf("Executing... ");  fflush( 0 );
  //start = clock();
  fftw_execute(p->plan); 
  //end = clock();
  //printf("Done. (time used %i)\n", end - start);  fflush( 0 );
  //printf("Clocks per sec... %i\n", CLOCKS_PER_SEC);
  
  // Fill the output back into the vector:
  for(i=0; i<len; i++) {
    //printf("Unloading: %lf\n", ((double*)out)[i]);
    Svector_set(vec, i, Sflonum(((double*)p->vec)[i]));
  }
}



//================================================================================
// Trying to read complex numbers even though they're not in the Chez API!



int test (int x) {
  return x+1;
}


ptr test2 (ptr x) {
  return Sflonum(3.8);
}

void query_type(ptr x) {
  printf("Object is a: ");
  if (Sinexactnump(x))  printf("inexact ");
  if (Sexactnump(x))  printf("exact ");
  if (Sfixnump(x))  printf("fixnum ");
  if (Scharp(x))  printf("char ");
  if (Snullp(x))  printf("nil ");
  if (Seof_objectp(x))  printf("eof ");
  if (Sbwp_objectp(x))  printf("bwp ");
  if (Sbooleanp(x))  printf("bool ");  
  if (Spairp(x))  printf("pair ");
  if (Ssymbolp(x))  printf("symbol ");
  if (Sprocedurep(x))  printf("procedure ");
  if (Sflonump(x))  printf("flonum ");
  if (Svectorp(x))  printf("vector ");
  if (Sstringp(x))  printf("string ");
  if (Sbignump(x))  printf("bignum ");
  if (Sboxp(x))  printf("box ");
  if (Sratnump(x))  printf("rational ");
  if (Sinputportp(x))  printf("inputport ");
  if (Soutputportp(x))  printf("outputport ");
  if (Srecordp(x))  printf("record ");
  printf("\n");  
}


void print_exact(ptr p) {
  printf("Got complex: %i\n", ((uptr)p) & ~0x3);

  printf("Sizeof ptr %i, uptr %i\n", sizeof(ptr), sizeof(uptr));

  unsigned long int x = *(long int *)((ptr)((uptr)p & ~0x3));
  unsigned long int y = *((long int *)(((uptr)p & ~0x3) + 1));
  printf("Dereferencing gives us: %i\n", x );
  printf("Dereferencing gives us: %i\n", y);


  printf("Dereferencing gives us bytes: %u %u %u %u   %u %u %u %u\n", 
	 *(((char *)((uptr)p & ~0x3)) + 0),
	 *(((char *)((uptr)p & ~0x3)) + 1),
	 *(((char *)((uptr)p & ~0x3)) + 2),
	 *(((char *)((uptr)p & ~0x3)) + 3),
	 *(((char *)((uptr)p & ~0x3)) + 4),
	 *(((char *)((uptr)p & ~0x3)) + 5),
	 *(((char *)((uptr)p & ~0x3)) + 6),
	 *(((char *)((uptr)p & ~0x3)) + 7)
	 );

}

//#define Sexactnump(x) ((((uptr)(x)&0x7)==0x7) &&\
//    ((uptr)((*((ptr *)((uptr)(x)+1))))==0xBE))


void print_inexact(ptr p) {
}
