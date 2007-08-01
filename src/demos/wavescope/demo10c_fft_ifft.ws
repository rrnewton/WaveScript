

// Ideally, the optimization would work automatically on these lifted versions:
//fun sigseg_fftR2C     (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)
//fun sigseg_ifftR2C    (ss) toSigseg(ss`toArray`ifftC2R, ss.start, ss.timebase)

winsize = 4096;

s0 = iterate _ in timer(1000.0) {
  state {cnt = 0.0}
  arr = Array:make(winsize,0.0);
  for i = 0 to winsize-1 {
    cnt += 1.0;    
    arr[i] := cnt;
  };
  emit arr;
}

BASE <- iterate a in s0 {
  emit a;
  emit ifftC2R( fftR2C(a) )
}

/*
s0 = (readFile("6sec_marmot_sample.raw", 
	       "mode: binary  rate: 24000  window: "++ winsize ++"  skipbytes: 6 ") :: Stream (Sigseg Int16));

BASE <- iterate w in s0 {
  arr = w`toArray;
  arr2 = Array:make(arr`Array:length, 0.0);
  for i = 0 to arr`Array:length -1 {
    arr2[i] := int16ToFloat$ arr[i];
  };
  emit ifftC2R( fftR2C(arr2) )
}
*/
