
include "marmot_first_phase.ws";

//======================================================================

/*
fftArray :: Array Float -> Array Complex;
fun fftArray(arr) {
  toArray(fft(toSigseg(arr, 0, nulltimebase)))
}
*/


/**** what if the input is not a power of 2.... fft?  ***/
FarFieldDOA :: (Stream (List (Sigseg t)), Array (Array Float)) -> Stream (Array (Array Float)); 
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
    Ndat = (m_in[0]).Array:length;  

    // This will be the output DOA likelihoods
    Jmet = matrix(NSrc,Ngrd,0.0);

    gnuplot_array(m_rowv_shared(m_in,0));

    //fft the sync'd data 
    ffts = m_rowmap(fftC2R, m_in);

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
    fun cmp(i,j) { (power[i] - power[j]) };
    sort(swap,cmp,power.Array:length);

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
doas = FarFieldDOA(synced, sensors);

BASE <- doas

