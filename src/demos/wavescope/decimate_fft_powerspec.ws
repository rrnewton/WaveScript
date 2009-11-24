

// Bring in the raw stream.
//s1 : Stream (Sigseg complex);
s1 = audio(0, 1024, 0);

// Decimate it.


// Run an FFT.
//s2 : Stream (Array complex);
s2 = iterate w in s1 {
  emit ss_fft (w);
};

// Finally, take the power spectrum.
//s3 : Stream float;
s3 = iterate w in s2 {
  arr = toArray w;

  if arr[100].realpart > 224192.0
  then { emit 0.0; emit arr[100].imagpart; }
  else { }
//  emit arr[100].realpart;
};

// Return the result to the user.
main = s3;
