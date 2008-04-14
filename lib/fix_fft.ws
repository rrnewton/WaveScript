/* fix_fft.c - Fixed-point in-place Fast Fourier Transform  */
/*
  All data are fixed-point short integers, in which -32768
  to +32768 represent -1.0 to +1.0 respectively. Integer
  arithmetic is used for speed, instead of the more natural
  floating-point.

  For the forward FFT (time -> freq), fixed scaling is
  performed to prevent arithmetic overflow, and to map a 0dB
  sine/cosine wave (i.e. amplitude = 32767) to two -6dB freq
  coefficients. The return value is always 0.

  For the inverse FFT (freq -> time), fixed scaling cannot be
  done, as two 0dB coefficients would sum to a peak amplitude
  of 64K, overflowing the 32k range of the fixed-point integers.
  Thus, the fix_fft() routine performs variable scaling, and
  returns a value which is the number of bits LEFT by which
  the output must be shifted to get the actual amplitude
  (i.e. if fix_fft() returns 3, each value of fr[] and fi[]
  must be multiplied by 8 (2**3) for proper scaling.
  Clearly, this cannot be done within fixed-point short
  integers. In practice, if the result is to be used as a
  filter, the scale_shift can usually be ignored, as the
  result will be approximately correctly normalized as is.

  Written by:  Tom Roberts  11/8/89
  Made portable:  Malcolm Slaney 12/15/94 malcolm@interval.com
  Enhanced:  Dimitrios P. Bouras  14 Jun 2006 dbouras@ieee.org
*/

//#include <stdio.h>
//#include <sys/types.h>
include "stdlib.ws"

N_WAVE      :: Int = 1024    /* full length of Sinewave[] */
LOG2_N_WAVE :: Int = 10      /* log2(N_WAVE) */

type Short = Int16;

/*
  Henceforth "short" implies 16-bit word. If this is not
  the case in your architecture, please replace "short"
  with a type definition which *is* a 16-bit word.
*/

/*
  Since we only use 3/4 of N_WAVE, we define only
  this many samples, in order to conserve data space.
*/
Sinewave :: Array Short = #[
      0,    201,    402,    603,    804,   1005,   1206,   1406,
   1607,   1808,   2009,   2209,   2410,   2610,   2811,   3011,
   3211,   3411,   3611,   3811,   4011,   4210,   4409,   4608,
   4807,   5006,   5205,   5403,   5601,   5799,   5997,   6195,
   6392,   6589,   6786,   6982,   7179,   7375,   7571,   7766,
   7961,   8156,   8351,   8545,   8739,   8932,   9126,   9319,
   9511,   9703,   9895,  10087,  10278,  10469,  10659,  10849,
  11038,  11227,  11416,  11604,  11792,  11980,  12166,  12353,
  12539,  12724,  12909,  13094,  13278,  13462,  13645,  13827,
  14009,  14191,  14372,  14552,  14732,  14911,  15090,  15268,
  15446,  15623,  15799,  15975,  16150,  16325,  16499,  16672,
  16845,  17017,  17189,  17360,  17530,  17699,  17868,  18036,
  18204,  18371,  18537,  18702,  18867,  19031,  19194,  19357,
  19519,  19680,  19840,  20000,  20159,  20317,  20474,  20631,
  20787,  20942,  21096,  21249,  21402,  21554,  21705,  21855,
  22004,  22153,  22301,  22448,  22594,  22739,  22883,  23027,
  23169,  23311,  23452,  23592,  23731,  23869,  24006,  24143,
  24278,  24413,  24546,  24679,  24811,  24942,  25072,  25201,
  25329,  25456,  25582,  25707,  25831,  25954,  26077,  26198,
  26318,  26437,  26556,  26673,  26789,  26905,  27019,  27132,
  27244,  27355,  27466,  27575,  27683,  27790,  27896,  28001,
  28105,  28208,  28309,  28410,  28510,  28608,  28706,  28802,
  28897,  28992,  29085,  29177,  29268,  29358,  29446,  29534,
  29621,  29706,  29790,  29873,  29955,  30036,  30116,  30195,
  30272,  30349,  30424,  30498,  30571,  30643,  30713,  30783,
  30851,  30918,  30984,  31049,  31113,  31175,  31236,  31297,
  31356,  31413,  31470,  31525,  31580,  31633,  31684,  31735,
  31785,  31833,  31880,  31926,  31970,  32014,  32056,  32097,
  32137,  32176,  32213,  32249,  32284,  32318,  32350,  32382,
  32412,  32441,  32468,  32495,  32520,  32544,  32567,  32588,
  32609,  32628,  32646,  32662,  32678,  32692,  32705,  32717,
  32727,  32736,  32744,  32751,  32757,  32761,  32764,  32766,
  32767,  32766,  32764,  32761,  32757,  32751,  32744,  32736,
  32727,  32717,  32705,  32692,  32678,  32662,  32646,  32628,
  32609,  32588,  32567,  32544,  32520,  32495,  32468,  32441,
  32412,  32382,  32350,  32318,  32284,  32249,  32213,  32176,
  32137,  32097,  32056,  32014,  31970,  31926,  31880,  31833,
  31785,  31735,  31684,  31633,  31580,  31525,  31470,  31413,
  31356,  31297,  31236,  31175,  31113,  31049,  30984,  30918,
  30851,  30783,  30713,  30643,  30571,  30498,  30424,  30349,
  30272,  30195,  30116,  30036,  29955,  29873,  29790,  29706,
  29621,  29534,  29446,  29358,  29268,  29177,  29085,  28992,
  28897,  28802,  28706,  28608,  28510,  28410,  28309,  28208,
  28105,  28001,  27896,  27790,  27683,  27575,  27466,  27355,
  27244,  27132,  27019,  26905,  26789,  26673,  26556,  26437,
  26318,  26198,  26077,  25954,  25831,  25707,  25582,  25456,
  25329,  25201,  25072,  24942,  24811,  24679,  24546,  24413,
  24278,  24143,  24006,  23869,  23731,  23592,  23452,  23311,
  23169,  23027,  22883,  22739,  22594,  22448,  22301,  22153,
  22004,  21855,  21705,  21554,  21402,  21249,  21096,  20942,
  20787,  20631,  20474,  20317,  20159,  20000,  19840,  19680,
  19519,  19357,  19194,  19031,  18867,  18702,  18537,  18371,
  18204,  18036,  17868,  17699,  17530,  17360,  17189,  17017,
  16845,  16672,  16499,  16325,  16150,  15975,  15799,  15623,
  15446,  15268,  15090,  14911,  14732,  14552,  14372,  14191,
  14009,  13827,  13645,  13462,  13278,  13094,  12909,  12724,
  12539,  12353,  12166,  11980,  11792,  11604,  11416,  11227,
  11038,  10849,  10659,  10469,  10278,  10087,   9895,   9703,
   9511,   9319,   9126,   8932,   8739,   8545,   8351,   8156,
   7961,   7766,   7571,   7375,   7179,   6982,   6786,   6589,
   6392,   6195,   5997,   5799,   5601,   5403,   5205,   5006,
   4807,   4608,   4409,   4210,   4011,   3811,   3611,   3411,
   3211,   3011,   2811,   2610,   2410,   2209,   2009,   1808,
   1607,   1406,   1206,   1005,    804,    603,    402,    201,
      0,   -201,   -402,   -603,   -804,  -1005,  -1206,  -1406,
  -1607,  -1808,  -2009,  -2209,  -2410,  -2610,  -2811,  -3011,
  -3211,  -3411,  -3611,  -3811,  -4011,  -4210,  -4409,  -4608,
  -4807,  -5006,  -5205,  -5403,  -5601,  -5799,  -5997,  -6195,
  -6392,  -6589,  -6786,  -6982,  -7179,  -7375,  -7571,  -7766,
  -7961,  -8156,  -8351,  -8545,  -8739,  -8932,  -9126,  -9319,
  -9511,  -9703,  -9895, -10087, -10278, -10469, -10659, -10849,
 -11038, -11227, -11416, -11604, -11792, -11980, -12166, -12353,
 -12539, -12724, -12909, -13094, -13278, -13462, -13645, -13827,
 -14009, -14191, -14372, -14552, -14732, -14911, -15090, -15268,
 -15446, -15623, -15799, -15975, -16150, -16325, -16499, -16672,
 -16845, -17017, -17189, -17360, -17530, -17699, -17868, -18036,
 -18204, -18371, -18537, -18702, -18867, -19031, -19194, -19357,
 -19519, -19680, -19840, -20000, -20159, -20317, -20474, -20631,
 -20787, -20942, -21096, -21249, -21402, -21554, -21705, -21855,
 -22004, -22153, -22301, -22448, -22594, -22739, -22883, -23027,
 -23169, -23311, -23452, -23592, -23731, -23869, -24006, -24143,
 -24278, -24413, -24546, -24679, -24811, -24942, -25072, -25201,
 -25329, -25456, -25582, -25707, -25831, -25954, -26077, -26198,
 -26318, -26437, -26556, -26673, -26789, -26905, -27019, -27132,
 -27244, -27355, -27466, -27575, -27683, -27790, -27896, -28001,
 -28105, -28208, -28309, -28410, -28510, -28608, -28706, -28802,
 -28897, -28992, -29085, -29177, -29268, -29358, -29446, -29534,
 -29621, -29706, -29790, -29873, -29955, -30036, -30116, -30195,
 -30272, -30349, -30424, -30498, -30571, -30643, -30713, -30783,
 -30851, -30918, -30984, -31049, -31113, -31175, -31236, -31297,
 -31356, -31413, -31470, -31525, -31580, -31633, -31684, -31735,
 -31785, -31833, -31880, -31926, -31970, -32014, -32056, -32097,
 -32137, -32176, -32213, -32249, -32284, -32318, -32350, -32382,
 -32412, -32441, -32468, -32495, -32520, -32544, -32567, -32588,
 -32609, -32628, -32646, -32662, -32678, -32692, -32705, -32717,
 -32727, -32736, -32744, -32751, -32757, -32761, -32764, -32766,
];



FIX_F2I :: (Float) -> Short;
fun FIX_F2I(a) ( (cast_num(a * 32767.0)::Short) ) 

FIX_I2F :: (Short) -> Float;
fun FIX_I2F(a) ( (cast_num(a)::Float) / 32767.0 )


/*
  FIX_MPY() - fixed-point multiplication & scaling.
  Substitute inline assembly for hardware-specific
  optimization suited to a particluar DSP processor.
  Scaling ensures that result remains 16-bit.
*/
FIX_MPY :: (Short, Short) -> Short;
fun FIX_MPY(a, b) {
  //  println("  MULT "++a++" "++b);

	/* shift right one less bit (i.e. 15-1) */
        _a :: Int32 = cast_num$ (a::Short);
        _b :: Int32 = cast_num$ (b::Short);

	//  println("  Shifting product "++ _a * _b);

	c = rshiftI32(_a * _b, 14);

	//  println("  Shifted by 14 "++ c);

	/* last bit shifted out = rounding-bit */
	// RRN: Looks like we could jump back to Int16 sooner:
	b = logandI32(c, 1);

	//  println("  Round bit "++ b);

	/* last shift + rounding bit */
	(cast_num(rshiftI32(c, 1) + b) :: Short)
}


FIX_DIV :: (Short, Short) -> Short;
fun FIX_DIV(x,y) {
  _x :: Int32 = cast_num$ (x::Short);
  _y :: Int32 = cast_num$ (y::Short);
  c :: Int32 = _x * 32767 / _y;

println("x "++x++" _x "++_x++" y "++y++" _y "++_y++
	   " shifted _x "++lshiftI32(_x, 16)++" div "++c);

  (cast_num$ (c)::Short)
}

/* doesnt work.?
FIX_SQRT :: (Short) -> Short;
fun FIX_SQRT(x) {
  fun newton(s) {
    next = rshiftI16(s + FIX_DIV(x, s),1);
    if (next == s) then s
    else newton(next)
  };	 
  newton(rshiftI16(x,1))
}
*/

FIX_SQRT :: (Short) -> Short;
fun FIX_SQRT(x) {
  s = Mutable:ref(x);
  next = Mutable:ref(rshiftI16(x,1));  
  while (s != next) {
    s := next;
    next := rshiftI16(s + FIX_DIV(x, s),1);
  };	 
  s
}


FIX_NORM :: (Short, Short) -> Short;
fun FIX_NORM(re, im) {
  FIX_SQRT(FIX_MPY(re,re)+FIX_MPY(im,im))
}


ln10 = logF(10.0);

FIX_LOG10 :: (Short) -> Short;
fun FIX_LOG10(f) {
  FIX_F2I(logF(FIX_I2F(f))/ln10)
}


/*
  fix_fft() - perform forward/inverse fast Fourier transform.
  fr[n],fi[n] are real and imaginary arrays, both INPUT AND
  RESULT (in-place FFT), with 0 <= n < 2**m; set inverse to
  0 for forward transform (FFT), or 1 for iFFT.
*/
fix_fft :: (Array Short, Array Short, Int, Bool) -> Int;
fun fix_fft(fr, fi, m, inverse)
{
  DEBUG = false;
        using Mutable;
  //	int mr, nn, i, j, l, k, istep, n, scale, shift;
  //	short qr, qi, tr, ti, wr, wi;

	n = lshiftI32(1, m);

	/* max FFT size = N_WAVE */
	if (n > N_WAVE.gint)
	then -1
        else { 

 	mr :: Ref Int32 = ref(0);
	nn = n - 1;
	scale = ref(0);

	if DEBUG then println("Input r: " ++ fr);
	if DEBUG then println("Input i: " ++ fi);

	/* decimation in time - re-order data */
	for m = 1 to (cast_num(nn)::Int) {
		l = ref(n);
		// Don't have do/while:
		l := rshiftI32(l,1);
		while (mr + l > nn) {
  		  l := rshiftI32(l,1);		  
		};
		mr := logandI32(mr, l-1) + l;
		if mr > m.gint then {
		  _mr :: Int = cast_num(mr);
		  // Do swaps:
		  tr      = fr[m];
		  fr[m]  := fr[_mr];
		  fr[_mr] := tr;

		  ti      = fi[m];
		  fi[m]  := fi[_mr];
		  fi[_mr] := ti;
		};
       	  if DEBUG then println("  dec loop r: " ++ fr);
	  if DEBUG then println("  dec loop i: " ++ fi);
	};

	if DEBUG then println("Decimated r: " ++ fr);
	if DEBUG then println("Decimated i: " ++ fi);

	l = ref(1);
	k :: Ref Int = ref(LOG2_N_WAVE - 1);

	while (l < n) {
		shift = ref(false);
		if inverse == true
		then {
			/* variable scaling, depending upon data */
			i = ref(0);
			while i.gint < n && not(shift) {
 			   j = absI16(fr[i]); 
			   m = absI16(fi[i]);
			   if (j > 16383 || m > 16383) 
			   then shift := true;
			};
			if shift then scale += 1;
		}
		 else {
			/*
			  fixed scaling, for proper normalization --
			  there will be log2(n) passes, so this results
			  in an overall factor of 1/n, distributed to
			  maximize arithmetic accuracy.
			*/
			shift := true;
		 };
		/*
		  it may not be obvious, but the shift will be
		  performed on each data point exactly once,
		  during this pass.
		*/
		istep = lshiftI32(l,1);

		for m = 0 to (cast_num(l)::Int) - 1 {

		  j :: Int = cast_num$ lshiftI32(m.gint, k);

 		  /* 0 <= j < N_WAVE/2 */
		  wr =  ref$ Sinewave[j + N_WAVE/4];
 	 	  wi =  ref$ 0-Sinewave[j];
		  if inverse then wi := 0-wi;
		  if shift then { 
		    wr := rshiftI16(wr,1);
		    wi := rshiftI16(wi,1);
	          };

		  //i = ref((cast_num(m)::Int));
		  i = ref(m);
		  while i.gint < n {
		    //j :: Int = cast_num$ i + l;
		    j :: Int = i + (cast_num(l)::Int);
		    tr = FIX_MPY(wr,fr[j]) - FIX_MPY(wi,fi[j]);
		    ti = FIX_MPY(wr,fi[j]) + FIX_MPY(wi,fr[j]);
		    qr = ref$ fr[(cast_num(i)::Int)];
		    qi = ref$ fi[(cast_num(i)::Int)];

		    if shift then {
		      qr := rshiftI16(qr,1);
		      qi := rshiftI16(qi,1);
		    };
	   	    fr[j] := qr - tr;
		    fi[j] := qi - ti;
		    fr[i] := qr + tr;
                    fi[i] := qi + ti;
	            i += (cast_num(istep) :: Int);
		  }
		}

		k += -1;
		l := istep;

	  //if DEBUG then println("  outer loop r: "++fr);
	  //if DEBUG then println("  outer loop i: "++fi);
	}

//	return scale;

	  0}
}



/*
main = iterate _ in COUNTUP(0) {
  state { x = 32767 }
  x := FIX_MPY(x,32400); 
  //x := FIX_MPY(x, 0); 

  using Array;

  //n = 256;
  n = 16;
  a = make(n,0);
  b = make(n,0);

  for i = 0 to n-1 {
    a[i] := FIX_F2I(sin((cast_num(i)::Float)*8.0*const_PI/(cast_num(n)::Float)));
    b[i] := 0;
  };

  for i = 0 to 15 { print(a[i]++" "); }; print("\n");
  for i = 0 to 15 { print(b[i]++" "); }; print("\n");

  print("FFT START\n");

  fix_fft(a, b, 4, false);

  print("FFT FINISHED: \n");

  t :: Int16 = 32023;
  println("Overflowed mult: "++ t*t);
  println("Overflowed add: " ++ t+t);

  s :: Uint16 = 32023;
  println("UOverflowed mult: "++ s*s);
  println("UOverflowed add: " ++ s+s+s);

  for i = 0 to 15 { print(a[i]++" "); }; print("\n");
  for i = 0 to 15 { print(b[i]++" "); }; print("\n");
  //println(Array:prefix(a,16));
  //println(b);


  quart = FIX_F2I(0.25);
  sq = FIX_SQRT(quart);

  quart2 = FIX_DIV(FIX_F2I(0.1),FIX_F2I(0.4));

  println("sqrt of "++FIX_I2F(quart)++" is "++FIX_I2F(sq));
  println(".1/.4 is "++FIX_I2F(quart2));


  emit ()
}

*/