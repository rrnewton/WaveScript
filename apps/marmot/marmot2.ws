
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

include "types.ws";
include "stdlib.ws";
include "matrix.ws";
//include "matrix_gsl.ws";

include "array_geometry.ws";

//======================================================================

sound_spd = 345.0; // HACK - although not quite sure how to put it in

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



// Accepts a matrix, and the associated theta and radius calculated, and returns the aml_vector
// grid_size is generally 360, for one-degree increments.
actualAML :: (Matrix Float, Array Float, Array Float, Int, Int) -> Array Float;
fun actualAML(data_in, radius, theta, grid_size, sens_num)
{
    using Matrix; using Complex;

    //log(1,"  Running actual AML.");

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
    fft_temp = fromArray2d(Float:rowmap(memoized_fftR2C, data_in));
    
    //    sel_bin_size = min(half_size,m_cols(fft_temp)/20); // the C version
    //sel_bin_size = min(total_bins, window_size/20);
    sel_bin_size = 100; // TEMP FIXME FIXME
    //    println("Sel_bin_size: " ++ sel_bin_size); // This is 100 in the C code!!

    // the above makes more sense when you're NOT using all bins for the AML (i.e. you're only using certain frequency bands)

    // the processed frequency data will be mapped into here
    data_f = create(sens_num, total_bins); // with no set values, yet. 4 channels x total bin size

    // Allocate some extra buffers that are used below.
    //D = Array:make(sens_num, (0::Complex));

    // set each element
    for i = 0 to (sens_num - 1) { // AML_NUM_CHANNELS
      for j = 0 to (total_bins - 1) { // window size
        set(data_f, i, j, sdivC(conjC( get(fft_temp,i,j)), intToFloat(window_size)) );
      };
      // set those first values - think this is supposed to be the last value in each array? - i is channel num
      //function takes the 0th element from the fft's imaginary and divides it by window size (setting it to the real), and sets the imag to 0 
      set(data_f, i, (total_bins-1), (1 * 
                                      floatToComplex(imagpart( get(fft_temp,i,0))/intToFloat(window_size))) );
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

    /* <<<<<< the actual sort >>>>>> */
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

    // [2008.07.29] Allocate once outside the loop:
    td = Array:makeUNSAFE(sens_num);

    strt = clock();
    /* <<<<<<<<<<<<<<<<<<<< ANGLE SEARCH >>>>>>>>>>>>>>>>>>>> */
    // now do the actual AML calculation, searching thru each angle
    for i = 0 to (grid_size - 1) {
      try_angle = intToFloat(i)*2.00*const_PI/intToFloat(grid_size);
      
      // TD code from C
      // td[k] = - radius[k] * cosf ( try_angle - theta[k])*param->samp_rate/param->sound_spd;

      // function to calculate time delay relative to centre of array
      fun delay(c) (0.00 - radius[c] * cos(try_angle - theta[c]) * samp_rate / sound_spd); 

      //td = Array:build(sens_num, delay);
      for k = 0 to sens_num-1 { td[k] := delay(k) };

      //      print("td = "++show(td[0])++" "++show(td[1])++" "++show(td[2])++" "++show(td[3])++"\n");
      for j = 0 to (sel_bin_size-1) {

        _order = i2f(order[j]);
	
	temp_c = ref(0);

	// [2008.07.29] This is the crux of the performance difference between gcc and MLton:
	for n = 0 to (sens_num - 1) {
	  _D       = expC2(2.0 * const_PI * _order * td[n] / _window_size);
	  temp_c  += conjC(_D) * get(data_f, n, order[j]);
	  Jvec[i] += norm_sqrC( _D * sdivC(temp_c, _sens_num) );

	  //fun sdivC(c,d) makeComplex(realpart(c)/d, imagpart(c)/d);
	  //fun norm_sqrC(c) (realpart(c) * realpart(c)) + (imagpart(c) * imagpart(c));

	  //foo = _D * temp_c;
	  //Jvec[i] += realpart(foo) + imagpart(foo);
	  //Jvec[i] += realpart$ sdivC(temp_c, _sens_num);
	  //Jvec[i] += realpart( _D * sdivC(temp_c, _sens_num));

	  //Jvec[i] := realpart(temp_c);
	};

	/*
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
	*/

      }
    };
    /* <<<<<<<<<<<<<<<<<<<< END ANGLE SEARCH >>>>>>>>>>>>>>>>>>>> */
    print("AML time "++ clock()-strt ++"\n");

    // gnuplot_array(Jvec);
    //    emit(Jvec); // and, we're done!
    // just return JVec, no emit!
    Jvec
}

//fun AML_prep1

//oneSourceAMLTD :: (Stream (List (Sigseg Float)), Matrix Float, Int) -> Stream (Array Float * Int64);
// does an AMl calc based on TD data of supplied window (i.e. it does no rewindowing itself)
// only does one source - other implementations may work on multiple sources

// This takes the floats directly, used in tests:
fun oneSourceAMLTD_helper(synced_floats, win_size) {
  using Matrix;
  using Float; 
  
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
  data_in = stream_map( fun(ls) {
                st = List:ref(ls,0)`start;
		tb = List:ref(ls,0)`timebase;
		wid = List:ref(ls,0)`width;
                log(1, "AMLING_DETECTION_SEGMENTS  START " ++st++ " WIDTH " ++wid++ " TIMEBASE " ++tb);
               (list_of_rowsegs_to_matrix(ls), st, tb)
	    },
	    synced_floats);

  // num_src = 1; // we're only interested in one source, this var is not used..
  grid_size = 360; // 1 unit per degree.
  //grid_size = 36; 

  // this is just one big iterate - there's only ever one iteration, so I'm assuming this is a convention to processing.. ?  
  aml_result = iterate (_m_in, starttime, tb) in data_in {

    // We extract a window of "win_size" to perform the AML algorithm on.
    // not doing any padding just yet - only do WHOLE windows

    offset = 0; // This is the offset into the original window.

    m_in :: Matrix Float = build(sens_num, win_size, fun(i,j) get(_m_in, i, j + offset));
    //   gnuplot_array(m_in[0]);

    //result = Mutable:ref(Array:null);
    //for i = 1 to 100 { result := actualAML(m_in, radius,theta, grid_size, sens_num) };
    //result = Array:make(grid_size, 0.0); // FAKE
    result = actualAML(m_in, radius,theta, grid_size, sens_num);

    log(1, "  Got result of AML.");

    //	gnuplot_array(result);
    emit(result, starttime, tb)
  };
  /*
  aml_result = iterate (z in aml_results) {
    emit z;
    };*/
  aml_result
}


// This wrapper also does the conversion to float:
oneSourceAMLTD :: (Stream Detection, Int) -> Stream AML;
fun oneSourceAMLTD(synced, win_size) {
  synced_floats = smap(fun(x) List:map(fun(x) sigseg_map(int16ToFloat,x), x), synced);
  oneSourceAMLTD_helper(synced_floats, win_size);
}



/**************************************************************/

// calculate normalised J (AML vector) values

// This basic version works fine... it just appeared not to work
// because of weird Gnuplot/mlton communication problems.
normalize_aml :: AML -> AML;
fun normalize_aml((doas,st,tb)) {
  total = Array:fold((+), 0.0, doas);
  arr = Array:map((/ total), doas);
  (arr, st, tb)
}

aml_to_unitcircle :: AML -> AML;
fun aml_to_unitcircle((doas,st,tb)) {
  high = Array:fold1(max, doas);
  arr = Array:map((/ high), doas);
  (arr, st, tb)
}

aml_to_int16s :: AML -> IntAML;
fun aml_to_int16s((arr,st,tb)) {
  //log(1,"Converting AML to Int16");
  fun convert(f) {
    //log(1,"Converting float! "++f);
    floatToInt16(f);
  };
  high = Array:fold(max, 0.0, arr);
  res = (Array:map(fun(n) convert((n / high) * 65535.0 - 32768.0), arr), st, tb);
  //log(1,"Finished AML conversion");
  res
}

// Remember to normalize after applying this!!
// This will produce numbers *between* 0.0 and 1.0, but they won't *sum* to 1.0
aml_to_floats :: IntAML -> AML;
fun aml_to_floats((arr,st,tb)) {
  (Array:map(fun(n) (int16ToFloat(n) + 32768.0) / 65535.0, arr), st, tb)
}


/**************************************************************/


// Little helper
fun segsToFloat(synced_ints)
  stream_map(fun (x) 
              map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);


// Takes a stream of detections
fun maybe_graph_aml(id, rotation, strm) {
  arrs = smap(fun((ar,_,_)) 
              Array:mapi(fun(i, radius) (const_PI * (i`i2f + rotation) / 180.0, radius), ar),
              strm);
  gui = Gnuplot:array_streamXY(
     "set title \"AML output node"++id++"\";\n"++
     "set polar;\n"++
     "set grid polar ;\n"++
     "unset border;\n"++
     "unset param;\n",
//     set angles DEGREES
     arrs);
  if GUIENABLED
  then merge(strm, iterate _ in gui {})
  else strm
}



fun draw_multi_amls(nodes,allamls)
iterate _ in 
Gnuplot:array_streamXY_multiplot(
   "", 
   map(fun((id,_,_,_))
      "set title \"AML output, node  "++(id::Int)++"\";\n"++
      "set polar;\n"++
      "set grid polar ;\n"++
      "set noxtics\n"++
      "set noytics\n"++
      "unset border;\n"++
      "unset param;\n"++
      "unset key;\n",
       nodes),
   map(fun(((id,_,_,rotation),strm)) 
       smap(fun((arr,_,_))
         Array:mapi(fun(i, radius) (const_PI * (i`i2f + rotation) / 180.0, radius), 
	     arr),
       smap(aml_to_unitcircle, strm)),
   List:zip(nodes,allamls))
) {}



fun draw_multi_detections(nodes,alldets)
iterate _ in 
Gnuplot:array_streamXY_multiplot(
   "", 
   map(fun((id,_,_,_))
      "set yrange [-5000:5000];\n"++
       "set noxtics\n"++
       "set noytics\n"++
      "set title \"Detection, node  "++id++"\";\n"++
      "set format x \"\";\n"++
      "unset border;\n"++
      "unset param;\n"++
      "unset key;\n"
      ,
       nodes),
   map(fun(((id,_,_,_), strm)) 
       smap(fun(sigsegs) {
  	   arr = toArray$ List:ref(sigsegs,0);
           println("DETECTIONMIN: "++Array:fold1(min,arr));	   
           Array:mapi(fun(i,x) 
	      if x == intToInt16(-32768) then  wserror("\n\nGOT LEAST NEGATIVE INT16!!!\n\n") else
	      if x < 0`gint then (i,absI16(x)) else (i,(x)), arr);
           //Array:mapi(fun(i,x) if x < 0`gint then (i,"-"++(1`gint - x)) else (i,show(x)), arr);
           //Array:mapi(fun(i,x) (i,max(x,gint(0))), arr);
	 },
       strm), 
   List:zip(nodes,alldets))
) {}



/*
// Takes a stream of detections
fun aml_tagged_detections(slsf) {
  strm = oneSourceAMLTD(smap(fun(_,x)x, slsf), 4096);
  arrs = smap(fun(((id,_,_,rot),(ar,_,_))) ar, strm);
  gui = Gnuplot:array_stream_autopolar("set title \"AML output\"\n",arrs);
  if GUIENABLED
  then merge(strm, iterate _ in gui {})
  else strm
}
*/
