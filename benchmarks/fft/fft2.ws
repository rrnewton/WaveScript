

// This is a port of the streamit benchmark.

include "stdlib.ws";


// ======================= SIMPLE HELPERS===========================
// Should be primitive.
fun remainder(n,m) { n - ((n/m) * m) }
fun repeatApply(n,f,x) {
  fun loop(n) if n==0 then x else f(loop(n-1));
  loop(n)
}
fun compose(f,g) fun(x) f(g(x))
fun composeAll(ls) 
  if ls == [] 
  then fun(x) x
  else if ls`tail == [] 
  then ls`head
  else List:fold1(compose, ls)
fun log2(n) {
  doubleToInt(logD(gint(n)) / logD(2))
}

// ==================================================================


// Take interleaved blocks of two samples each, and deinterleave.
//FFTReorderSimple :: (Int, Stream Float) -> Stream Float;
//FFTReorderSimple :: (Int, Stream (Sigseg Float)) -> Stream (Sigseg Float);
//fun FFTReorderSimple(n) fun (strm) {
fun FFTReorderSimple(n, strm) {
  totalData = 2*n;
  //dewindow $ iterate ss in window(strm, totalData) {
  iterate ss in rewindow(strm, totalData, 0) {
  //iterate ss in strm {

    //println("Firing reord simple "++n++" "++ss[[0]]);

    direct = toArray(ss);
    arr1 = Array:build(n, fun(i) direct[i/2 * 4 + remainder(i,2)]);
    arr2 = Array:build(n, fun(i) direct[i/2 * 4 + remainder(i,2) + 2]);

/*     println("  "++n++" Got direct, length "++direct`Array:length++ */
/*             " arr1/2: "++Array:length(arr1)++" "++Array:length(arr2) */
/*             ++"  start "++ss`start); */

    emit toSigseg(arr1, ss`start, ss`timebase);     // start is bogus
    emit toSigseg(arr2, ss`start + to64(n), ss`timebase); 

    //emit toSigseg(Array:append(arr1,arr2), 0, nulltimebase)
  }
}

//FFTReorder :: (Int, Stream Float) -> Stream Float;
fun FFTReorder(n,strm) {
  half = n/2;
  fun loop(i, acc) {
    if i >= half then acc
    else loop(i*2, FFTReorderSimple(n/i, acc))
  };
  result = loop(1, strm);
  final = rewindow(result, n*2, 0);
  result;
}


//CombineDFT :: (Int, Stream Float) -> (Stream Float);
//CombineDFT :: (Int, Stream (Sigseg Float)) -> (Stream (Sigseg Float));
fun CombineDFT(n, strm) {  
/*   work push 2*n pop 2*n { */
  if n<2 then wserror("CombineDFT, window size must be greater than 1");
  //iterate ss in strm {
  iterate ss in rewindow(strm, 2*n, 0) {
  //dewindowArrays $ iterate ss in window(strm, n*2) {
    // coefficients, real and imaginary interleaved
    state { 
      //results = Array:make(2*n, 0.0);
     w = { 
      wn_r :: Float = cos(2.0 * 3.141592654 / gint(n));
      wn_i :: Float = sin(-2.0 * 3.141592654 / gint(n));
      arr = Array:make(n, 0.0);
      fun loop(i, real,imag) {
        if i >= n then arr
        else {
	  arr[i]   := real;
	  arr[i+1] := imag;
	  next_real :: Float = real * wn_r - imag * wn_i;
	  next_imag :: Float = real * wn_i + imag * wn_r;
	  loop(i+2, next_imag, next_imag)
	}
      };
      loop(0, 1.0, 0.0)
    }}

    println("Running combine n "++n++"  w[1]="++w[1]++"  ss[[0]]="++ss[[0]]);

    inp = ss`toArray;
    results = Array:make(2*n, 0.0);
    last = 2*n-1;
    for i0 = 0 to (n/2)-1 {
      i = i0*2;
      i_plus_1 = i+1;

      y0_r = inp[last-i];
      y0_i = inp[last-i_plus_1];

      y1_r = inp[last-(n + i)];
      y1_i = inp[last-(n + i_plus_1)];

      weight_real = w[i];
      weight_imag = w[i_plus_1];

      y1w_r = y1_r * weight_real - y1_i * weight_imag;
      y1w_i = y1_r * weight_imag + y1_i * weight_real;

      results[last-i]       := y0_r + y1w_r;
      results[last-(i + 1)] := y0_i + y1w_i;

      results[last-(n + i)]     := y0_r - y1w_r;
      results[last-(n + i + 1)] := y0_i - y1w_i;

      //emit results;
      emit toSigseg(results,ss`start,ss`timebase);
     }
  }
}

//FFTKernel2 :: (Int, Stream Float) -> Stream Float;
fun FFTKernel2(n,strm) {
  fn1 = fun(s) FFTReorder(n,s);
  // Create a reorder followed by a number of combine's:  
  fn2 = composeAll $ List:reverse $
        List:build(log2(n), fun(i) fun(s) CombineDFT(exptI(2,i+1),s));
  fn3 = compose(fn2, fn1);
  //snoop("RoundRobOUT:", roundRobinMap(2, fn1, strm));
  // [2007.12.02] Can't split it right now because it yields gapped windowed streams.
  //roundRobinMap(2, fun(s) window(fn3(dewindow(s)), 2*n), window(strm, 2*n));
  fn3(strm)
}

fun FFTTestSource(n) {
  iterate _ in  timer(10.0) {
    print("FFT Source Emit!!\n");
    emit 0.0;
    emit 0.0;
    emit 1.0;
    emit 0.0;
    for i = 1 to 2*(n-2) {
      emit 0.0;
    }
  }
}

fun FFTTestSourceWindowed(n) {
  iterate _ in  timer(10.0) {      
    state { cnt = 0 }
    arr = Array:make(2*n, 0.0);
    arr[2] := 1.0;
    cnt += to64(2*n);
    emit toSigseg(arr,cnt,nulltimebase);
  }
}



//================================================================================
// FINAL RETURN STREAM:

BASE <- dewindow $ FFTKernel2(64, FFTTestSourceWindowed(64));

//BASE <- FFTKernel2(8, FFTTestSource(8));
//BASE <- FFTKernel2(64, window(FFTTestSource(64), 128));
//BASE <- FFTKernel2(64, FFTTestSource(64));

/*
BASE <- iterate _ in timer(10.0) {
  println("Log2 of 16: "++log2(16));
  emit 0;
}
*/
