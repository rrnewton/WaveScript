
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.
// Was that with a quadruple FFT?

// run ./get_sample_data first

// [2007.03.22] Wow, just for reading streams of data a tuple at a
// time... there's a lot of scheduler overhead.
//
// For example, just returning "s1" from this program on a 1.2 mb file:
//   ws     : 364 ms (from saved heap, using scheme fft)
//   wsc    : 1300-2900 ms (high variance)
//   wscaml : 13 ms

// Then returning 's2' which includes fft:
//   ws     : 772 ms (from saved heap, using scheme fft - 460 minus startup and w/ fftw)
//   wsc    : 2000 ms (high variance)
//   wscaml : 31 ms
// (Total process times.)

fun sigseg_fftC   (ss) toSigseg(ss`toArray`fftC,    ss.start, ss.timebase)
fun sigseg_ifftC  (ss) toSigseg(ss`toArray`ifftC,   ss.start, ss.timebase)
fun sigseg_fftR2C (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)
fun sigseg_ifftC2R(ss) toSigseg(ss`toArray`ifftC2R, ss.start, ss.timebase)

fun sigseg_map (f, ss) {
  arr = Array:build(ss.width, fun(i) f(ss[[i]]));
  toSigseg(arr, ss.start, ss.timebase)
}

fun mywindow(S, len)
  iterate(x in S) {
    state{
      arr = Array:null;
      ind = 0;
      startsamp = 0;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x);
      startsamp := startsamp + len;
    }
  };

s1 :: Stream (Sigseg Float);
s1 = if GETENV("WSARCH") != "ENSBox" 
     then {chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) 
                    :: Stream (Int16 * Int16 * Int16 * Int16));
	   mywindow(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 4096) }
     else ENSBoxAudio(0,4096,0,24000);

//if GETENV("WSARCH") == "ENSBox" 

/*
fun copy(arr) Array:build(arr`Array:length, fun(i) arr[i]);
fun conj(c) c - (gint(2) * (0.0+1.0i * floatToComplex(imagpart(c))));
fun manual_double(ss) {
  src = ss `toArray;
  len1 = ss.width;
  len2 = 2 * (len1 - 1);
  arr = Array:build(len2, 
    fun(i) 
      if i < len1 
      then src[i]
      else conj $ src[len2-i] );
  toSigseg(arr, ss.start, ss.timebase);
}
*/

s2 :: Stream (Sigseg Complex);
s2 = iterate (w in s1) {
  //state{ foo = (Array:null :: Array Int);   }
  //print(foo);  print("\n");
  
  //a = manual_double $ sigseg_fftR2C (w) ;
  //b = sigseg_fftC   $ sigseg_map(floatToComplex, w);
  //inspect $ toArray $ a;
  //inspect $ toArray $ b;
  //inspect $ toArray(a)==toArray(b);

  //  inspect $ toArray $ sigseg_ifftC2R $ sigseg_fftR2C (w) ;
  //  inspect $ toArray $ sigseg_ifftC   $ sigseg_fftC $ sigseg_map(floatToComplex, w);

  emit sigseg_fftR2C(w);
  //  emit sigseg_fftC( sigseg_map(floatToComplex, w));
  // Now roundtrip with the full complex transform:
  //  emit sigseg_fftC( sigseg_ifftC( sigseg_fftC( sigseg_map(floatToComplex, w))));
  // Now roundtrip with the restricted real transform:
  //emit sigseg_fftR2C( sigseg_ifftC2R( sigseg_fftR2C (w)))


  /*
  emit sigseg_map(floatToComplex, w);
  emit sigseg_map(floatToComplex, w);
  emit sigseg_ifftC( sigseg_fftC( sigseg_map(floatToComplex, w)));
  fun chopfront(ss) subseg(ss, ss.start, 20);
  //    emit sigseg_map(floatToComplex, sigseg_ifftC2R( sigseg_fftR2C (w)));
  print(chopfront( sigseg_ifftC2R( sigseg_fftR2C (w))) ++ "\n");
  */

};

//s3 :: Stream Float;
s3 = iterate (win in s2) {
  state { pos=0 }

  x :: Int = 3;  // Explicit type annotation on local var.
  y = (4 == 4);

  ind = 100;

  if win[[ind]].realpart > 224.0
  then { //emit 0.0; 
    emit (pos/4, win[[ind]].imagpart)
  };

  pos += 1;
};

BASE <- 
s3//mywindow(s3, 4)
//s1
//iterate(x in s2) { emit x[[30]] };
//iterate(x in s1) { emit x`width };
