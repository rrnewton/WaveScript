
#include <complex.h>
#include <fftw3.h>
#include <time.h>

#include <scheme.h>
#include <stdio.h>



// NOT THREADSAFE!
fftw_plan the_plan = 0;
fftw_complex* the_vec = 0;
int the_vec_len = 0;

void set_fftw_plan_dft_1d (int N) {
  printf("Setting plan! %i  %i  %i\n", the_plan, the_vec, N);
  
  if (the_plan != 0) {
    printf("Wiping before setting...\n");
    fftw_destroy_plan(the_plan);
    fftw_free(the_vec);
    the_vec_len = 0;
  }
  the_vec = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);  
  the_vec_len = N;
  the_plan = fftw_plan_dft_1d(N, the_vec, the_vec, FFTW_FORWARD, FFTW_ESTIMATE);  
  printf("Plan set..\n");
}
void clear_fftw_plan_dft_1d () {
  if (the_plan != 0) {
    fftw_destroy_plan(the_plan);
    fftw_free(the_vec);
    the_vec_len = 0;
  }
}

int make_fftw_plan_dft_1d (int N) {
  printf("Creating plan! %i\n", N);

  fftw_plan plan;
  fftw_complex* vec = 0;
  int vec_len = 0;
  
  vec = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);  
  vec_len = N;
  plan = fftw_plan_dft_1d(N, vec, vec, FFTW_FORWARD, FFTW_ESTIMATE);
  
  return (int)plan;
}

void free_fftw_plan_dft_1d (int plan) {

  fftw_destroy_plan((fftw_plan)plan);
  //fftw_free(the_vec);
  //the_vec_len = 0;
}




void s_fftw_execute (ptr vec) {
  int i;
  int len = Svector_length(vec);
  int N = len / 2;  
  clock_t start, end;

  // TODO: CHECK THAT LENGTH IS RIGHT!
  if (N != the_vec_len)
    printf("Mismatched lengths! %i %i\n", N, the_vec_len);

  //fftw_complex *in, *out;
  //fftw_plan p;
  //in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  //out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  //out = in;
 
  //printf("Measuring... ");  fflush( 0 );
  //start = clock();
  //  p = fftw_plan_dft_1d(N, in, in, FFTW_FORWARD, FFTW_ESTIMATE);
  //p = fftw_plan_dft_1d(N, in, out, FFTW_FORWARD, FFTW_MEASURE);
  //p = fftw_plan_dft_1d(N, in, out, FFTW_FORWARD, FFTW_PATIENT);
  //end = clock();
  //printf("Done. (time used %i)\n", end - start);  fflush( 0 );

  //printf("Filling... current vals: %i %i %i\n", the_plan, the_vec, the_vec_len);  fflush( 0 );  
  for(i=0; i<len; i+=2) {
    /*printf("Loading: real %lf, imag %lf\n",
	   Sflonum_value(Svector_ref(vec, i)),
	   Sflonum_value(Svector_ref(vec, i+1)));*/

    ((double*)the_vec)[i]   = Sflonum_value(Svector_ref(vec, i));
    ((double*)the_vec)[i+1] = Sflonum_value(Svector_ref(vec, i+1));
  }
  //printf("Done.\n");  fflush( 0 );
 
  //printf("Executing... ");  fflush( 0 );
  //start = clock();
  fftw_execute(the_plan); /* repeat as needed */
  //end = clock();
  //printf("Done. (time used %i)\n", end - start);  fflush( 0 );
  
  //printf("Clocks per sec... %i\n", CLOCKS_PER_SEC);

  //fftw_destroy_plan(p);
  
  // Fill the output back into the vector:
  for(i=0; i<len; i++) {
    //printf("Unloading: %lf\n", ((double*)out)[i]);
    Svector_set(vec, i, Sflonum(((double*)the_vec)[i]));
  }
  
  //fftw_free(in); 
  //  fftw_free(out);
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
