
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.
// Was that with a quadruple FFT?

s1 :: Signal (Sigseg Float);
s1 = audio(0, 1024, 0);

s2 :: Signal (Sigseg Complex);
s2 = iterate (w in s1) {
  emit fft(w);
};

s3 :: Signal Float;
s3 = iterate (win in s2) {
  x : Int = 3;  // Explicit type annotation on local var.
  y = (4 == 4);

  if win[[100]].realpart > 224.0
  then { emit 0.0; emit win[[100]].imagpart; }
  else { }
};


BASE <- s3;
