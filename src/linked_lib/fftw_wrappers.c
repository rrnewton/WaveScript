
// [2008.01.27] This version is for the C-backend.

// Wrappers for functions from fftw.
// Only included when the corresponding wavescript functions are invoked.

// FIXME TODO: MAKE THREAD SAFE!!
//Mutex planner_lock;

int        initialized = 0;
int        last_plan_size = 0;
fftwf_plan cached_plan;


int        last_ifft_size = 0;
int        initialized_ifft = 0;
fftwf_plan cached_ifft_plan;

//================================================================================
  static void memoized_fftR2C(float* in_buf, float complex* out_buf) {
      int len = ARRLEN(in_buf);
      // thread safe initialization
      if (initialized == 0) {
        // grab the lock
#ifndef DISABLE_THREADS
	//        planner_lock.lock();
#endif
        // ok.. if we have the lock then either someone already initialized it
        // ahead of us, or we must init it.  if initialized is still 0...
        if (initialized == 0) {
          initialized = 1;
          fprintf(stderr, "  Allocating fftw plan for the first time, size %d\n", len);
  	  fflush(stderr);
  	  last_plan_size = len;
	  // FFTW_MEASURE, FFTW_PATIENT, FFTW_EXHAUSTIVE
	  cached_plan = fftwf_plan_dft_r2c_1d(len, (float*)0, (fftwf_complex*)0, FFTW_ESTIMATE | FFTW_UNALIGNED);
        }
#ifndef DISABLE_THREADS
	//   planner_lock.unlock();
#endif
      }
      if (last_plan_size != len) {
        fprintf(stderr, "ack, we're screwed.. fftw plan for %d, not %d\n",
		last_plan_size, len);
	exit(-1);
      }
      //fprintf(stderr, "   EXECUTING PLAN size %d\n", len);
      fftwf_execute_dft_r2c(cached_plan, in_buf, (fftwf_complex*)out_buf);
  }


//================================================================================
  static void memoized_ifftC2R(float complex* in_buf, float* out_buf) {
      int len = ARRLEN(in_buf);
      //int len_out = (len - 1) * 2;
      // thread safe initialization
      if (initialized_ifft == 0 || last_ifft_size != len) {
        // grab the lock
#ifndef DISABLE_THREADS
	//        planner_lock.lock();
#endif
        if (initialized_ifft == 0) {
          initialized_ifft = 1;
          fprintf(stderr, "  (re)Allocating ifftw plan, size %d\n", len); 
        } else fftwf_destroy_plan(cached_ifft_plan);
	last_ifft_size = len;
	cached_plan = fftwf_plan_dft_c2r_1d(len, (fftwf_complex*)0, (float*)0, FFTW_ESTIMATE | FFTW_UNALIGNED);
#ifndef DISABLE_THREADS
	//   planner_lock.unlock();
#endif
      }
      fftwf_execute_dft_c2r(cached_plan, (fftwf_complex*)in_buf, out_buf);
  }

