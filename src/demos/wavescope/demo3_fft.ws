
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.
// Was that with a quadruple FFT?

s1 : Stream (Sigseg float);
s1 = audio(0, 1024, 0);

s2 : Stream (Sigseg complex);
s2 = iterate (w in s1) {
  emit fft(w);
};

s3 : Stream float;
s3 = iterate (win in s2) {
  x : int = 3;
  y = (4 == 4);

  if win[[win.start + 100]].realpart > 224192.0
  then { emit 0.0; emit win[[win.start + 100]].imagpart; }
  else { }
};


BASE <- s3;
