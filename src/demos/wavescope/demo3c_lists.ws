



// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));
s1 = timer(1000.0);

s2 = iterate( w in s1 ) {
  //  emit [w.width, w.start];
  emit [3085];
};

s3 = iterate( ls in s2) {
  //emit (ls.head, ls.tail);
  emit ls.head;
}

main = s3;

