
/* 
  RRN: Trying to analyze the poor performance.
  Takes 190 ms user time on testdata.txt.
  Takes 36 ms when discounting the AML itself.
    (Slow text reading, lots of alloc?)
  Leaving out BOTH the sort loop and the angle search: 36 ms 
  The sort loop is also trivial... all the time's in the angle search.  
  Building the delay array.... trivial cost.

  Hmm... allocating 'D' in the inner loop... that looks killer.
  Uncommenting it doesn't add significant cost... but it might get DCE'd.

  That ONE LINE where we initialize the steering vector does almost all the damage. (Brings us up to 180ms)
  Doing expC2 by itself (make complex plus two trig ops) causes 40ms damage.
  Just ALLOCATING a complex number (with floatToComplex) does just as much damage.  
  
  Wait... that large floating point expression (-2.0 * ...) ALSO seems to account for nearly all of the time in question.
  AH, tricky... maybe it's stopping the array TD from being dead-code-eliminated.
  No.. if I remove the reference to td... it still takes a while.
  In fact, *just* referencing td costs almost nothing.
  Hmm, accessing "order[j]", and converting it to float... *that's* expensive.
  INTERESTING!!! the intToFloat is implemented very badly!!!!!

  After fixing conversions, the multiply expression that was painful before is now trivial.
  In fact, now the expC is pretty cheap (~10ms)! It was the conversions that were expensive!
  (Moving where we allocate the matrices makes no difference really... MLton must fix that.)

  GREAT!  That original D[n] = line that was causing trouble... it now causes 20X less trouble.
  So we have got a 2.5x improvement (total) from fixing the conversions.
  We get an additional 28% from improving expC to not be wasteful with complex operations and allocations.
  That brings us to a total time of 55ms on my 17inch macbook currently.

NEXT ROUND:

 Lifting: I attempted several lifts manually... lifted conversion
  functions (order[j], window_size) out of the innermost loop.  I also
  lifted the allocation of the matrix D.  None of this gave me ANY performance benefit.
  I should undo it for clarity, but I'm going to leave it in case we end up running with wsc.

 Fixing complex operators: fixed up sdivC and conjC, slight gain, down to 52ms.
  (Need to run this more times to get more reasonable numbers!)

STILL TO DO:

  * Split the array D into real and imaginary arrays 
  * allocate the array td once and just refill it.

GREAT: I scaled it up, and when we exec 100 reps we got these times:
  * C version - 3.09  seconds
  * wsmlton   - 1.67  seconds

RUNNING ON ENSBOX:
  * farfield_ex : 2.0 user
  * 

 
*/


//include "rewindowGeneral.ws";
//include "run_aml_test.ws";

include "stdlib.ws";
include "matrix.ws";
//include "matrix_gsl.ws";

include "array_geometry.ws";

//======================================================================

samp_rate = 24000.0; // HACK - we should get this from the stream/timebase/sigseg
sound_spd = 345.0; // HACK - although not quite sure how to put it in

sensors = Matrix:Float:fromList2d(sensor_list);

// This doesn't seem quite worthy of going in the standard library yet:
//list_of_rowsegs_to_matrix :: List (Sigseg t) -> Matrix t
fun list_of_rowsegs_to_matrix(ls) {
  using Matrix; using Float;
  r = List:length(ls);
  fst = ls`head;
  c = width(fst);
  // Here we should create_UNSAFE...
  //mat = create(r,c, fst[[0]]);
  mat = create(r,c);

  fun copyrow(i, ss) { for j = 0 to c-1 { set(mat, i,j, ss[[j]]) } };
  List:foreachi(copyrow, ls);
  mat
}

// rrn: TEMP: this is getting around a problem with Array:build and foreign
// functions and metaprogram evaluation.
fun mybuild(len, f) {
  arr = Array:makeUNSAFE(len);
  for i = 0 to len-1 {
    arr[i] := f(i);
  };
  arr
}

//metabuild = mybuild;
metabuild = Array:build;


/*
  ptr = ref(ls);
  for i = 0 to r-1 {
    hd = ptr`head;
    for j = 0 to c-1 {
      set(mat, i,j, hd[[j]])
    };
    ptr := ptr`tail;
  };
  mat 
  */


// reference single-target AML representation, based on the aml.c file in emstar
// implemented by Mike Allen, July 2007

//fun sdivC(c,d) (1.0+0.0i * floatToComplex(realpart(c)/d)) + (0.0+1.0i * floatToComplex(imagpart(c)/d));
fun sdivC(c,d) makeComplex(realpart(c)/d, imagpart(c)/d);

fun norm_sqrC(c) (realpart(c) * realpart(c)) + (imagpart(c) * imagpart(c));

// equivalent to the expC implementation in stdlib.ws
fun expC2(f) makeComplex(cos(f), sin(f))


// does an aml given fftd data already.. assumes windows of data are already fftd
//fun oneSourceAMLFFT(synced, sensors)

//Accepts a matrix, and the associated theta and radius calculated, and returns the aml_vector

actualAML :: (Matrix Float, Array Float, Array Float, Int, Int) -> Array Float;
fun actualAML(data_in, radius, theta, grid_size, sens_num)
{
    using Matrix; using Complex;

    log(1,"Running actual AML.");

    _ = (data_in :: Matrix Float);

    // so we can use m_rowmap to map our function in the same way as the fft
    //window_size = (data_in[0])`Array:length; // this is the size of one of the rows in m_in, right? currently 16384 - WHY
    window_size = snd(Float:dims(data_in));
    _window_size = i2f(window_size);
    _sens_num = i2f(sens_num);

    // total bins that will come out of the FFT
    total_bins = window_size/2;

    // The result vector
    Jvec = Array:make(grid_size, 0.0);

    //fft the sync'd data - these must be channels, otherwise the fft doesn't make any sense
    fft_temp = fromArray2d(Float:rowmap(fftR2C, data_in));
    
    //    sel_bin_size = min(half_size,m_cols(fft_temp)/20); // the C version
    //sel_bin_size = min(total_bins, window_size/20);
    sel_bin_size = 100; // TEMP FIXME FIXME
    //    println("Sel_bin_size: " ++ sel_bin_size); // This is 100 in the C code!!

    // the above makes more sense when you're NOT using all bins for the AML (i.e. you're only using certain frequency bands)

    // the processed frequency data will be mapped into here
    data_f = create(sens_num, total_bins); // with no set values, yet. 4 channels x total bin size

    // Allocate some extra buffers that are used below.
    D = Array:make(sens_num, 0.0+0.0i);

    // set each element
    for i = 0 to (sens_num - 1) { // AML_NUM_CHANNELS
      for j = 0 to (total_bins - 1) { // window size
        set(data_f, i, j, sdivC(conjC( get(fft_temp,i,j)), intToFloat(window_size)) );
      };
      // set those first values - think this is supposed to be the last value in each array? - i is channel num
      //function takes the 0th element from the fft's imaginary and divides it by window size (setting it to the real), and sets the imag to 0 
      set(data_f, i, (total_bins-1), (1.0+0.0i * floatToComplex(imagpart( get(fft_temp,i,0))/intToFloat(window_size))) );
      // set (channel, element in array)
    };
  
    // this sort is directly taken from the aml.c implementation
    // instead of sorting all the bins, only a subset are sorted == quicker, I think
  
    psds = Array:make(total_bins,0.0); // power across bins
    psd_index = Array:make(total_bins,0); // power indices
    
    for j = 0 to (total_bins-1) {
      for i = 0 to (sens_num-1) { // AML_NUM_CHANNELS
	psds[j] := psds[j] + norm_sqrC( get(data_f,i,j)); // data_f is channels as rows, freq data as cols 
      };
      psd_index[j] := j;
    };
    
    temp_val = ref(0.0); 
    temp_ind = ref(0);
    max_ind = ref(0);
    order = Array:make(sel_bin_size,0); // order of array

    // the actual sort
    for i = 0 to (sel_bin_size-1) {
      temp_val := psds[i];
      temp_ind := psd_index[i];
      max_ind := i;
      
      for j = i+1 to (total_bins-1) {
	if psds[j] > temp_val then {
	  max_ind := j;
	  temp_val := psds[j];
	  temp_ind := psd_index[j];
	};
      };
      
      if (max_ind < i || max_ind > i) then { // i don't know the syntax for != here
	psds[max_ind] := psds[i];
	psd_index[max_ind] := psd_index[i];
      	psds[i] := temp_val;
	psd_index[i] := temp_ind;
      };      
      order[i] := temp_ind; // all subsequent access to psds is done thru the order array
    };


    //    gnuplot_array(order);
    // now do the actual AML calculation, searching thru each angle
    for i = 0 to (grid_size - 1) {
      try_angle = intToFloat(i)*2.00*const_PI/intToFloat(grid_size);
      
      // TD code from C
      // td[k] = - radius[k] * cosf ( try_angle - theta[k])*param->samp_rate/param->sound_spd;

      // function to calculate time delay relative to centre of array
      fun delay(c) (0.00 - radius[c] * cos(try_angle - theta[c]) * samp_rate / sound_spd); 

      td = Array:build(sens_num, delay);

      //      print("td = "++show(td[0])++" "++show(td[1])++" "++show(td[2])++" "++show(td[3])++"\n");
      for j = 0 to (sel_bin_size-1) {

        _order = i2f(order[j]);
	
	temp_c = ref(0.0+0.0i);

	// compute steering vector D (steering vector lines up channels, a la delay and sum beamforming)
	for n = 0 to (sens_num - 1) {
           // took out the order[j]+1 (seems to make it equal)
	   // odd thing here is that if we take out the - from -2.0, the results are correct.. need to figure this out

	   // RRN: This inner loop was costing us dearly before, improved to this:
	   D[n] := expC2(2.0 * const_PI * _order * td[n] / _window_size);
	};

	for n = 0 to (sens_num - 1) {
	  temp_c := temp_c + conjC(D[n]) * get(data_f, n,order[j]);
	};

	for n = 0 to (sens_num - 1) {
	  Jvec[i] := Jvec[i] + norm_sqrC( (D[n] * sdivC(temp_c, _sens_num)) );
	  // c version is : Cnormsqr(Cmul(temp_c,D[k])), where temp_c is divided by AML_NUM_CHANNELS
	}
      }

    };

    // gnuplot_array(Jvec);
    //    emit(Jvec); // and, we're done!
    // just return JVec, no emit!
    Jvec
}

oneSourceAMLTD :: (Stream (List (Sigseg Float)), Matrix Float, Int) -> Stream (Array Float);
// does an AMl calc based on TD data of supplied window (i.e. it does no rewindowing itself)
// only does one source - other implementations may work on multiple sources

// win_size decides how AML results to use
fun oneSourceAMLTD(synced, sensors, win_size)
{
  using Matrix; using Float; 
  // calculate how many acoustic sensors exist (this is AML_NUM_CHANNELS)
  // rrn: can't currently calculate matrix dimensions (foreign function) at compile time:
  sens_num = List:length(sensor_list);

  // RRN: NOTE:   Here we are (potentially) calling foreign matrix functions at compile time...
  // RRN: Instead, we make sure to access the *list* of sensors here, not the *matrix*:

  // build an array with sens_num sensors in it for theta and radius (polar coords)
  // 1. radius  
  radius = metabuild(sens_num, fun(i) sqrtF( sqr( List:ref(List:ref(sensor_list,i),0)) 
                                           + sqr( List:ref(List:ref(sensor_list,i),1))));
  // 2. theta
  theta = metabuild(sens_num, fun(i) atan2( List:ref(List:ref(sensor_list,i),1), 
                                            List:ref(List:ref(sensor_list,i),0)));

  //  print(show(get(sensors,0,0))++"\n");

  //  print("radius = "++show(radius[0])++" "++show(radius[1])++" "++show(radius[2])++" "++show(radius[3])++"\n");
  //  print("theta = "++show(theta[0])++" "++show(theta[1])++" "++show(theta[2])++" "++show(theta[3])++"\n");

  // convert the data from a list of segs into a matrix
  data_in = stream_map(list_of_rowsegs_to_matrix, synced);

  // num_src = 1; // we're only interested in one source, this var is not used..
  grid_size = 360; // 1 unit per degree.

  // this is just one big iterate - there's only ever one iteration, so I'm assuming this is a convention to processing.. ?  
  aml_result = iterate (_m_in :: Matrix Float in data_in) {

    log(1, "Computing AML result.");

    // We extract a window of "win_size" to perform the AML algorithm on.
    // not doing any padding just yet - only do WHOLE windows

    offset = 0; // This is the offset into the original window.

    m_in :: Matrix Float = build(sens_num, win_size, fun(i,j) get(_m_in, i, j + offset));
    //   gnuplot_array(m_in[0]);

    //result = Mutable:ref(Array:null);
    //for i = 1 to 100 { result := actualAML(m_in, radius,theta, grid_size, sens_num) };
    //result = Array:make(grid_size, 0.0); // FAKE
    result = actualAML(m_in, radius,theta, grid_size, sens_num);

    //	gnuplot_array(result);
    emit(result)
  };
  /*
  aml_result = iterate (z in aml_results) {
    emit z;
    };*/
  aml_result
}

