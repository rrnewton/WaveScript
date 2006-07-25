
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.

s1 : Stream (Sigseg complex);
s1 = audio(0, 1024, 0);

s2 : Stream (Array complex);
s2 = iterate (w in s1) {
  emit fft(to_array(w));
};

s3 : Stream float;
s3 = iterate (arr in s2) {  
  x : int = 3;
  if arr[100].realpart > 224192.0
  then { emit 0.0; emit arr[100].imagpart; }
  else { }
//  emit arr[100].realpart;
};


BASE <- s3;
