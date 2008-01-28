
// [2008.01.27] This version is for the C-backend.

// Wrappers for functions from fftw.
// Only included when the corresponding wavescript functions are invoked.

// FIXME TODO: MAKE THREAD SAFE!!
//Mutex planner_lock;


// Need to implement the memoized version to play nicer with thread safety.

int        initialized = 0;
int        last_plan_size = 0;
fftwf_plan cached_plan;
//float*     cached_inbuf;
//complex*   cached_outbuf;

static void memoized_fftR2C(float* in_buf, complex* out_buf) {
      int len = ARRLEN(in_buf);
      int len_out = (len / 2) + 1;     
      //complex* out_buf = malloc(len_out * sizeof(complex));

      // thread safe initialization
      if (initialized == 0) {

        // grab the lock
#ifndef BOOST_SP_DISABLE_THREADS
	//        planner_lock.lock();
#endif

        // ok.. if we have the lock then either someone already initialized it
        // ahead of us, or we must init it.  if initialized is still 0...

        if (initialized == 0) {
          fprintf(stderr, "  Allocating fftw plan for the first time, size %d\n", len);
  	  fflush(stderr);
  	  last_plan_size = len;
	  //cached_inbuf  = (float*)fftwf_malloc(len     * sizeof(float));
	  //cached_outbuf = (complex*)fftwf_malloc(len_out * sizeof(complex));
	  //cached_plan = fftwf_plan_dft_r2c_1d(len, cached_inbuf, (fftwf_complex*)cached_outbuf, FFTW_ESTIMATE | FFTW_UNALIGNED);

	  // FFTW_MEASURE, FFTW_PATIENT, FFTW_EXHAUSTIVE
          cached_plan = fftwf_plan_dft_r2c_1d(len, (float*)0, (fftwf_complex*)0, FFTW_ESTIMATE | FFTW_UNALIGNED);
          // OK it is init'd now
          initialized = 1;
        }

#ifndef BOOST_SP_DISABLE_THREADS
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

      printf("  WRITING INTO %p len %d [500] %d\n", out_buf, ARRLEN(out_buf), __real__ out_buf[500]);
      //      return out_buf;
   }


/*    // [2007.10.28] Porting to new Array format. */
/*    // Could generate this within emit-c.ss using FFTW + codegen for arrays. */

/*    static floatarr ifftC2R(complexarr& input) { */
/*       int len = input->len; */
/*       int len_out = (len - 1) * 2;  */

/*       wscomplex_t*   in_buf  = (wscomplex_t*)input->data;  */
/*       wsfloat_t  *   out_buf = new wsfloat_t[len_out]; // Return a new array with the result. */

/*       //printf(" FFT: %d->%d\n", len, len_out); */

/*       // Inefficient!  This approach makes a new plan every time. */
/*       // Real to complex: */
/*       fftwf_plan plan = fftwf_plan_dft_c2r_1d(len, (fftwf_complex*)in_buf, out_buf, FFTW_ESTIMATE);   */
/*       fftwf_execute(plan); */
/*       fftwf_destroy_plan(plan);            */

/*       //WSArrayStruct<wscomplex_t>* result = (WSArrayStruct<wscomplex_t>*)malloc(sizeof(WSArrayStruct<wscomplex_t>)); */
/*       WSArrayStruct<wsfloat_t>* result = new WSArrayStruct<wsfloat_t>; */
/*       result->rc = 0; */
/*       result->len = len_out; */
/*       result->data = out_buf; */
/*       return floatarr(result); */
/*    } */


