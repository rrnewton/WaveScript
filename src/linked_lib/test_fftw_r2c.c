

#define SIZE 4

#include <fftw3.h>
int main() {
      float start[SIZE];
      int i;
      
      for (i=0; i<SIZE; i++) start[i] = (float)(i+1);
      
      printf("Starting out: ");
      for (i=0; i<SIZE; i++) printf(" %f ", (start[i])); printf("\n");

      _Complex float middle[SIZE/2 + 1];
      
      fftwf_plan plan = fftwf_plan_dft_r2c_1d(SIZE, start, (fftwf_complex*)middle, FFTW_ESTIMATE);
      fftwf_execute(plan);
      fftwf_destroy_plan(plan);

      printf("Done with forward transform: ");
      for (i=0; i< SIZE/2 + 1; i++) printf(" %g+%gi ", __real__ (middle[i]),  __imag__ (middle[i]));
      printf("\n");

      float end[SIZE];

      fftwf_plan plan2 = fftwf_plan_dft_c2r_1d(SIZE, (fftwf_complex*)middle, end, FFTW_ESTIMATE);
      fftwf_execute(plan2);
      fftwf_destroy_plan(plan2);

      printf("Done with reverse transform transform: ");
      for (i=0; i<SIZE; i++) printf(" %fi ", (end[i]));
      printf("\n");
}


/*
Starting out:  1.000000i  2.000000i  3.000000i  4.000000i
Done with forward transform:  10+0i  -2+2i  -2+0i
Done with reverse transform transform:  4.000000i  8.000000i  12.000000i  16.000000i

*/
