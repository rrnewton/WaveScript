


// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0);

s2 = iterate( w in s1 ) {
  state{ ht = hashtable(5) }
  
  ht := hashset_BANG(ht, (w.start, w.end), w.start);

  emit (w, ht);
};

s3 = iterate( (w, ht) in s2) {
  emit hashget(ht, (w.start, w.end));
}

BASE <- s3;

