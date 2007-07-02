

// Wrappers for functions from fftw.

#include <fftw3.h>
#include "platform.h"

/*
Int32 testo(Real32 blah) {
  return (Int32)blah + 10;
}
*/

void raw_fftR2C(Pointer input, Pointer output, Int32 len) {
      float* in_buf           = (float*)          input;
      _Complex float* out_buf = (_Complex float*) output;

      //      wscomplex_t* out_buf = new wscomplex_t[len_out];

      int len_out = (len / 2) + 1;     
      
      //      printf(" FFT: %d->%d\n", len, len_out);

      // Inefficient!  This approach makes a new plan every time.
      // Real to complex:

      // [2007.07.02] Duh, fftw will INTERNALLY cache the last plan:
      fftwf_plan plan = fftwf_plan_dft_r2c_1d(len, in_buf, (fftwf_complex*)out_buf, FFTW_ESTIMATE);
      fftwf_execute(plan);
      fftwf_destroy_plan(plan);           
 }


int        last_plan_size = 0;
fftwf_plan cached_plan;
float*     cached_inbuf;
_Complex float*   cached_outbuf;


// Not having luck with this:
/*
void set_cached_plan( Pointer outbuf, Int32 len ) {
   printf("  Setting cached... %p %d\n", outbuf, len);
   if (last_plan_size != 0) {
        printf("  Destroying old...\n");	
      	fftwf_destroy_plan(cached_plan);
	fftwf_free(cached_inbuf);
	// We don't dealloc the outbuf because that's managed by MLton:
	//fftwf_free(cached_outbuf);
   }
   last_plan_size = len;
   cached_inbuf  = fftwf_malloc(len     * sizeof(float));
   cached_outbuf = (_Complex float*)outbuf;
   //cached_outbuf  = fftwf_malloc((len/2 + 1) * sizeof(_Complex float));
   cached_plan = fftwf_plan_dft_r2c_1d(len, cached_inbuf, (fftwf_complex*)cached_outbuf, FFTW_ESTIMATE);
}

// This assumes tha the plan has alreday been set.
void memoized_fftR2C(Pointer input, Int32 len) {
      printf("Calling fftw... in %p out %p len %d lastplan %d\n", input, cached_outbuf, len, last_plan_size);
      // This is the cost of doing things this way.  
      // The input/output buffers must stay constant.
      // So we must copy the input data.
      memcpy(input, cached_inbuf, len * sizeof(float));
      fftwf_execute(cached_plan);
 }
*/

// Sigh, for now doing a copy on input and output.  Above I tried to only do copy on input.
// But I ran into strange segfaults when allocating FFTW's output buffer in MLton...
// The weird thing is that the original raw_fftR2C *does* allocate both input and output buffers in MLton.
Pointer memoized_fftR2C(Pointer input, Int32 len) {
  //      float* in_buf = (float*)input;      
      int len_out = (len / 2) + 1; 
           
      if (last_plan_size == 0) {
        fprintf(stderr, "Allocating fftw plan for the first time, size %d\n", len);
      } else if (last_plan_size != len) {
        fprintf(stderr, "REALLOCATING cached fftw plan, size %d\n", len);
      	fftwf_destroy_plan(cached_plan);
	fftwf_free(cached_inbuf);
	fftwf_free(cached_outbuf);
      }

      if (last_plan_size != len) {
	last_plan_size = len;
	cached_inbuf  = fftwf_malloc(len     * sizeof(float));
	cached_outbuf = fftwf_malloc(len_out * sizeof(_Complex float));	
	// FFTW_MEASURE, FFTW_PATIENT, FFTW_EXHAUSTIVE
        cached_plan = fftwf_plan_dft_r2c_1d(len, cached_inbuf, (fftwf_complex*)cached_outbuf, FFTW_PATIENT);
      }

      // This is the cost of doing things this way.  
      // The input/output buffers must stay constant.
      memcpy(cached_inbuf, input, len * sizeof(float));
      fftwf_execute(cached_plan);
      //      memcpy(output, cached_outbuf, len_out * sizeof(_Complex float));
      return cached_outbuf;
 }




/*
int main ()
{
  return 0;
}
*/
