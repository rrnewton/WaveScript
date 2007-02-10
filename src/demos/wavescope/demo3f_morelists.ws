



// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0, 44000);

s2 = iterate( w in s1 ) {
  emit [w.start, w.end];
};

s3 = iterate( w in window(s2, 2)) {
  emit(append(w[[0]], w[[1]]));
}

BASE <- s3;

