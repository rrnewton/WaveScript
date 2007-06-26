

// Wrappers for functions from fftw.

#include <fftw3.h>
#include "platform.h"

Int32 testo(Real32 blah) {
  return (Int32)blah + 10;
}


static void fftR2C(Pointer input, Pointer output, Int32 len) {
      float* in_buf           = (float*)          input;
      _Complex float* out_buf = (_Complex float*) output;

      //      wscomplex_t* out_buf = new wscomplex_t[len_out];

      int len_out = (len / 2) + 1;     
      
      printf(" FFT: %d->%d\n", len, len_out);

      // Inefficient!  This approach makes a new plan every time.
      // Real to complex:
      fftwf_plan plan = fftwf_plan_dft_r2c_1d(len, in_buf, (fftwf_complex*)out_buf, FFTW_ESTIMATE);
      fftwf_execute(plan);
      fftwf_destroy_plan(plan);           

 }


/*
int main ()
{
  return 0;
}
*/
