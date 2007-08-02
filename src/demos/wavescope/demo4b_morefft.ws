
// This tests variants of fft, including the inverse ffts.
/***********************************************************************/

include "stdlib.ws";

//if GETENV("WSARCH") == "ENSBox" 


fun copy(arr) Array:build(arr`Array:length, fun(i) arr[i]);
fun conj(c) c - (gint(2) * (0.0+1.0i * floatToComplex(imagpart(c))));

// Double the half-sized complex representation to the same size as the real representation.
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

fun assert(str,b) if not(b) then wserror("Assert failed: "++ str ++"\n");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);
fun assert_prnt(str,pred) {
  assert(str,pred);
  print("Assert passed: "++ str ++ "\n");
}
fun assert_eq_prnt(str,a,b) {
  assert_eq(str,a,b);
  print("Assert passed: "++ str ++ "\n");
}

fun arrdiffF(a1,a2) 
  Array:build(Array:length(a1), fun (i) absF(a1[i] - a2[i]))
fun arrdiffC(a1,a2) 
  Array:build(Array:length(a1), fun (i) absC(a1[i] - a2[i]))

fun closeenough(arr1, arr2) {
  Array:andmap(fun (delt) delt < 1.0, arrdiffC(arr1,arr2));
}


s1a = if GETENV("WSARCH") != "ensbox" 
     then {chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) 
                    :: Stream (Int16 * Int16 * Int16 * Int16));
	   window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 4096) }
     else ensBoxAudioF(0);

s0 = (readFile("6sec_marmot_sample.raw", 
               "mode: binary  rate: 24000  window: 4096  skipbytes: 6 ") :: Stream (Sigseg Int16));
s1b = iterate w in s0 {
  arr = Array:build(w.width, fun (i) int16ToFloat(w[[i]]));
  emit toSigseg(arr, w.start, nulltimebase)
}

s1 = s1b;

//s2 :: Stream (Sigseg Complex);
s2 = iterate (w in s1) {
  
  println("\n  Roundtripping with the restricted real transform...");
  trip = sigseg_ifftC2R( sigseg_fftR2C (w));
  round1 = Array:map(roundF, w`toArray);
  round2 = Array:map(roundF, trip`toArray);
  
  for i = 0 to 4 { print(round1[i]++"   ") }; print("\n");
  for i = 0 to 4 { print(round2[i]++"   ") }; print("\n");
  
  assert_prnt("Roundtrip gets us back", round1 == round2);
  print("\n");

  once = sigseg_fftR2C (w);
  thrice = sigseg_fftR2C( sigseg_ifftC2R( sigseg_fftR2C (w)));

  for i = 0 to 4 { print(roundF(absC(once[[i]]))++"   ") }; print("\n");
  for i = 0 to 4 { print(roundF(absC(thrice[[i]]))++"   ") }; print("\n");

  assert_prnt("Roundtrip on the complex side", 
              closeenough(once`toArray, thrice`toArray));
  print("\n");

  emit ();

  /*
  diff1 = arrdiffC(once`toArray, trip`toArray);
  inspect$ diff1;

  a = manual_double $ sigseg_fftR2C (w) ;
  b = sigseg_fftC   $ sigseg_map(floatToComplex, w);  


  for i = 0 to 4 { print(a[[i]]++" \n") }; print("\n");
  for i = 0 to 4 { print(b[[i]]++" \n") }; print("\n");




  inspect $ toArray $ a;
  inspect $ toArray $ b;

  inspect $ arrdiffC(a`toArray,b`toArray);

  assert_prnt("real->complex and complex->complex match:", 
              closeenough(a`toArray,b`toArray));
  
  inspect $ toArray(a)==toArray(b);

  inspect $ toArray $ sigseg_ifftC2R $ sigseg_fftR2C (w) ;
  inspect $ toArray $ sigseg_ifftC   $ sigseg_fftC $ sigseg_map(floatToComplex, w);

  emit sigseg_fftR2C(w);
  //  emit sigseg_fftC( sigseg_map(floatToComplex, w));
  // Now roundtrip with the full complex transform:
  emit sigseg_fftC( sigseg_ifftC( sigseg_fftC( sigseg_map(floatToComplex, w))));

  /*
  emit sigseg_map(floatToComplex, w);
  emit sigseg_map(floatToComplex, w);
  emit sigseg_ifftC( sigseg_fftC( sigseg_map(floatToComplex, w)));
  fun chopfront(ss) subseg(ss, ss.start, 20);
  //    emit sigseg_map(floatToComplex, sigseg_ifftC2R( sigseg_fftR2C (w)));
  print(chopfront( sigseg_ifftC2R( sigseg_fftR2C (w))) ++ "\n");
  */

*/
};


/*
// Emit a number drawn from a fixed position in the fft output.
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
*/

BASE <- 
//s3
s2
//s3
//s1
//mywindow(s3, 4)
//s1
//iterate(x in s2) { emit x[[30]] };
//iterate(x in s1) { emit x`width };
