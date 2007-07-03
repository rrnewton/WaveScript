
include "marmot_first_phase.ws";

//======================================================================

/*
fftArray :: Array Float -> Array Complex;
fun fftArray(arr) {
  toArray(fft(toSigseg(arr, 0, nulltimebase)))
}
*/


fun sdivC(c,d) (1.0+0.0i * floatToComplex(realpart(c)/d)) + (0.0+1.0i * floatToComplex(imagpart(c)/d));

fun norm_sqrC(c) (realpart(c) * realpart(c)) + (imagpart(c) * imagpart(c));

fun FarFieldDOAb(synced, sensors)
{
  // we don't define window size in the Wavescope version, because the sigsegs imply the size of the AML (potentially)
  // therefore one would re-window the data as it came in to be AML'd

  sens_num = m_rows(sensors); // calculate how many sensors to be used (normally 4)

  samp_rate = 24000.0;
  sound_spd = 345.0;

  // do some kind of re-window function here
  //rwsynced = rewindow(synced, 4096, 0); // no gap, 4096 sample windows

  // build an array with sens_num sensors in it for theta and radius (polar coords)
  // 1. radius
  radius = Array:build(sens_num, fun(i) sqrtF(sqr(m_get(sensors,i,0) + sqr(m_get(sensors,i,1)))));
  // 2. theta
  theta = Array:build(sens_num, fun(i) atan2(m_get(sensors,i,1), m_get(sensors,i,0)));

  // convert the data from a list of segs into a matrix
  data_in = stream_map(list_of_segs_to_matrix, synced);
  //  data_in = stream_map(list_of_segs_to_matrix, rwsynced);

  num_src = 1;
  grid_size = 360;

  //  test = window(data_in, 4096);

  // this is just one big iterate - there's only ever one iteration, so I'm assuming this is a convention to processing.. ?  
  aml_result = iterate (m_in in data_in) {

    // so we can use m_rowmap to map our function in the same way as the fft
    Ndat = (m_in[0])`Array:length; // this is the size of one of the rows in m_in, right? currently 16384 - WHY
    // used to be Ndat 
    total_bins = Ndat/2;

    //    Jmat = matrix(num_src,grid_size,0.0); // the aml J vector - number of sources by grid size

    Jvec = Array:make(grid_size, 0.0);

    // steering vector of complex numbers
    D = Array:make(sens_num, 0.0+0.0i);

    //fft the sync'd data - these must be channels, otherwise the fft doesn't make any sense
    fft_temp = m_rowmap(fftR2C, m_in); // just maps per-row, as you would expect ends up being 8192 samples (complex, no redundancy)

    //    sel_bin_size = min(half_size,m_cols(fft_temp)/20); // hacked from Andreas' code
    sel_bin_size = min(total_bins, Ndat/20);

    // all channels, half the size, starting at 1 not 0
    // this does ONE channel : performs a conjugation on each element in the array, and then returns it
    data_f = matrix(sens_num, total_bins, 0.00+0.00i); // with no set values, yet. 4 channels x total bin size

    // set each element
    for i = 0 to (sens_num - 1) {
      for j = 0 to (total_bins - 1) {
        set(data_f, i, j, sdivC(conjC(m_get(fft_temp,i,j)), intToFloat(Ndat)) );
      };
      // set those first values - think this is supposed to be the last value in each array? - i is channel num
      set(data_f, i, (total_bins-1), (1.0+0.0i * floatToComplex(imagpart(m_get(fft_temp,i,0))/intToFloat(Ndat))) );
      //function takes the 0th element from the fft's imaginary and divides it by window size (setting it to the real), and sets the imag to 0 
    };

    //gnuplot_array(fft_temp[0]);

    /*        
    // compute psds
    fun norm2(x) { sqr(absC(x)) };
    //    psds = m_map(norm2, fft_temp);
    psds = m_map(norm2, data_f);

    //    gnuplot_array(m_rowv_shared(psds,0));

    // calculate norms (for each channel) - only need this in conjunction with normalising the data
    //    fun tmp(row) { sqrtF(asum(row)) };
    //    norms = m_rowmap_scalar(tmp, psds);

    // normalize
    //    fun normalize(row,i) { amult_scalar(row,gint(1)/f2c(norms[i])) };
    //    nffts = m_rowmap_index(normalize, ffts);

    // sum powers values across channels     
    power = Array:make(psds[0]`Array:length, 0.0);
    for i = 0 to (psds`Array:length - 1) {
      for j = 0 to (psds[0]`Array:length - 1) {
        power[j] := power[j] + m_get(psds,i,j);
      }
    };
    */

    psds = Array:make(total_bins,0.0); // power across bins
    psd_index = Array:make(total_bins,0); // power indices
    
    for i = 0 to (total_bins-1) {
      for j = 0 to (sens_num-1) {
	psds[i] := psds[i] + norm_sqrC(m_get(data_f,j,i)); // data_f is channels as rows, freq data as cols 
      };
      psd_index[i] := i;
    };
    //gnuplot_array(psds);
    /*
    // just sort the indices..
    ordered = Array:make(total_bins,0);
    
    temp_val = Array:make(1,0.0); // HACK
    temp_ind = Array:make(1,0); // HACK
    max_ind = Array:make(1,0); // HACK
    
    for i = 0 to (total_bins-1) {
      temp_val[0] := psds[i];
      temp_ind[0] := i;
      max_ind[0] := i;
      for j = 0 to (total_bins-1) {
	if psds[j] > temp_val[0] then {
	  max_ind[0] := j;
	  temp_val[0] := psds[i];
	}
      }
      
      //if (max_ind != i) {
      //psds[max_ind] := psds[i];
      //	psds[i] := temp_val;
      //}      
      //order[i] := temp_ind;
    };
    */
    
    //    gnuplot_array(power);

    // create index vector    
    //power_index = Array:make((m_in[0]).Array:length, 0);
    //for i = 0 to power_index.Array:length-1 { power_index[i] := i; };

    // sort index vector and power vector
    
    fun swap(i,j) { 
      tmp1 = psd_index[i]; 
      psd_index[i] := psd_index[j];
      psd_index[j] := tmp1; 
      tmp2 = psds[i]; 
      psds[i] := psds[j];
      psds[j] := tmp2; 
    };
    fun cmp(i,j) { floatToInt (psds[i] - psds[j]) };
    sort(swap, cmp, total_bins);
    
    /*
    fun swap(i,j) { 
      tmp1 = power_index[i]; 
      power_index[i] := power_index[j];
      power_index[j] := tmp1; 
      tmp2 = power[i]; 
      power[i] := power[j];
      power[j] := tmp2; 
    };
    fun cmp(i,j) { floatToInt (power[i] - power[j]) };
    sort(swap, cmp, power`Array:length);
    */
    //gnuplot_array(psds);
    
    for i = 0 to (grid_size - 1) {
      try_angle = intToFloat(i)*2.00*const_PI/intToFloat(grid_size);

      // function to calc time delay      
      fun delay(c) {
	0.00 - radius[c] * cos( try_angle - theta[c]) * samp_rate / sound_spd;
      };

      td = Array:build(sens_num, fun(c) delay(c));

      temp_c = Array:make(1,0.0+0.0i); // HACK :-)

      for j = 0 to (sel_bin_size-1) {
	// compute steering vector D	
	for n = 0 to (sens_num - 1) {
	  D[n] := expC(0.0+1.0i * f2c(-2.0 * const_PI * gint(psd_index[n]+1) * td[n] / gint(Ndat)));
	};
	
	for n = 0 to (sens_num - 1) {
	  temp_c[0] := temp_c[0] + conjC(D[n]) * m_get(data_f,n,psd_index[j]);
	};

	for n = 0 to (sens_num - 1) {
	  Jvec[i] := Jvec[i] + norm_sqrC( (D[n] * sdivC(temp_c[0], intToFloat(sens_num))) );
	};
      };
    };
    
    //gnuplot_array(Jvec);

    // tests with emit
                emit(Jvec);
    //    emit(data_in`Stream:length);
  };

  aml_result
  //  data_in`Array:length
}

/**** what if the input is not a power of 2.... fft?  ***/
//FarFieldDOA :: (Stream (List (Sigseg t)), Array (Array Float)) -> Stream (Array (Array Float)); 
fun FarFieldDOA(synced, sensors)
{	
  // parameters:
  Ngrd = 360;           // Ngrd is the number of grid points to compute DOA for.
  NSrc = 5;             // maximum number of sources to consider
  Nsel = 30;            // number of frequency bins to consider
  MaxIter = if NSrc == 1 then 1 else 5; // maximum number of ot

  Nsens = m_rows(sensors);


  /* compute r and theta for each sensor relative to sensor 0 as origin */
  r = Array:build(Nsens, 
		 fun(i) sqrtF(sqr(m_get(sensors,i,0) - m_get(sensors,0,0)) +
			      sqr(m_get(sensors,i,0) - m_get(sensors,0,1)) +
			      sqr(m_get(sensors,i,2) - m_get(sensors,0,2))));
  theta = Array:build(Nsens,
		     fun(i) atan2(m_get(sensors,i,1) - m_get(sensors,0,1), 
				  m_get(sensors,i,0) - m_get(sensors,0,0)));
		     
  // gridmap maps polar grid entry to radians
  gridmap = Array:build(Ngrd, 
                       fun(i) deg2rad(intToFloat(i) * 360.0 / i2f(Ngrd)));

  // convert the output of sync to a matrix.. yes this kind of sucks 
  matrix_in = stream_map(list_of_segs_to_matrix, synced);

  // ok, i guess we do one big iterate.. 
  result = iterate (m_in in matrix_in) {
    // length of the fft
    Ndat = (m_in[0])`Array:length;  

    // This will be the output DOA likelihoods
    Jmet = matrix(NSrc,Ngrd,0.0);

    gnuplot_array(m_rowv_shared(m_in,0));

    //fft the sync'd data 
    ffts = m_rowmap(fftR2C, m_in);

    // compute psds
    fun norm2(x) { sqr(absC(x)) };
    psds = m_map(norm2, ffts);

    // compute norms: sqrt(sum(psds))
    // BTW, using psds is hand-optimized form of computing norms
    fun tmp(row) { sqrtF(asum(row)) };
    norms = m_rowmap_scalar(tmp, psds);

    // normalize
    fun normalize(row,i) { amult_scalar(row,gint(1)/f2c(norms[i])) };
    nffts = m_rowmap_index(normalize, ffts);


    // sum powers values across channels 
    power = Array:make(Ndat, 0.0);
    for i = 0 to psds.Array:length-1 {
      for j = 0 to Ndat-1 {
        power[j] := power[j] + m_get(psds,i,j);
      }
    };

    // create index vector
    power_index = Array:make((m_in[0]).Array:length, 0);
    for i = 0 to power_index.Array:length-1 { power_index[i] := i; };

    // sort index vector and power vector
    fun swap(i,j) { 
      tmp1 = power_index[i]; 
      power_index[i] := power_index[j];
      power_index[j] := tmp1; 
      tmp2 = power[i]; 
      power[i] := power[j];
      power[j] := tmp2; 
    };
    fun cmp(i,j) { floatToInt (power[i] - power[j]) };
    sort(swap, cmp, power`Array:length);

    // T is the maximum direction grid value
    T = a_ones(NSrc);
    Tbefore = Array:make(NSrc, 0.0); 
    Trad = Array:make(NSrc, 0.0);

    // phase delays for each source
    delay = matrix(Nsens, NSrc, 0.0);

    // steering matrix
    D = matrix(NSrc, Nsens, 0.0+0.0i);

    fun doDelay(P, grid) {
      0.0 - r[P] * cos(gridmap[grid] - theta[P])
    };

    // 5 iterations
    for iter = 1 to MaxIter {
      for Q = 0 to NSrc-1 {
	
	J = Array:make(Ngrd, 0.0);

	// compute delay for other sources
	for L = 0 to NSrc-1 {
	  if L != Q then {
	     for P = 1 to Nsens-1 {
	       m_set(delay,P,L,doDelay(P, T[L]));
	    }
	  }
	};
	
	for I = 0 to Ngrd-1 {
	  
	  // compute delay for this direction
	  for P = 1 to Nsens-1 {
	    m_set(delay,P,Q,doDelay(P,I));
	  };

	  // Sum over highest power frequecies
	  for K = 0 to Nsel-1 {

	    // calculate the steering matrix
	    for L = 0 to NSrc-1 {
	      if (iter > 1 || L <= Q) then {
		m_set(D,0,L, 1.0+0.0i);
		for P = 1 to Nsens-1 {
		  m_set(D,P,L, 
                        expC(0.0+1.0i * 
                             f2c(-2.0 * const_PI
		                 * gint(power_index[K]+1)
                                 * m_get(delay,P,L) 
                                 / gint(Ndat))));
		}
	      }
	    };

	    // get col vector from matrix
	    X = m_colm(nffts,power_index[K]);

	    Post = m_mult(D, m_invert(D));
	    J[I] := J[I] + realpart(m_get(m_mult(m_trans(X), m_mult(Post, X)),0,0));
	  }
	};


	let (maxJ, maxJind) = a_max(J);
	T[Q] := maxJind;
        Trad[Q] := gint(T[Q]-1) * 2.0 * const_PI / gint(Ngrd);
        
	for K = 0 to m_cols(Jmet) {
          m_set(Jmet,Q,K,(J[K] / maxJ));
	}
      };

   /*
      // stopping condition, break out of loop?
      diffs = Array:make(Trad.Array:length, 0.0); //gint(0));
      for K = 0 to Trad.Array:length-1 { diffs[K] := absF(Tbefore[K]-Trad[K]) };
      let (max_change,_) = a_max(diffs);

      //or, in matlab... that would just be 'max(abs(Tbefore-Trad))'!!!
      
      if (max_change < 0.001) then {
        break;
      };

      for i = 0 to Tbefore.Array:length - 1 {
	Tbefore[i] := gridmap[T[i]] 
      }

  */
    };

    emit(Jmet);

  };

  result
}

//========================================
// Main query:

/* define array geometry */
sensors = list_to_matrix([[ 0.4,-0.4,-0.4],
			  [ 0.4, 0.4, 0.4],
			  [-0.4, 0.4,-0.4],
			  [-0.4,-0.4, 0.4]]);

// 'synced' is defined in marmot_first_phase.ws
doas = FarFieldDOAb(synced, sensors);

BASE <- doas

