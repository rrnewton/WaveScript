



// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0);

s2 = iterate( w in s1 ) {
  //  emit [w.width, w.start];
  emit [3085];
};

s3 = iterate( ls in s2) {
  //emit (ls.head, ls.tail);
  emit ls.head;
}

BASE <- s3;

