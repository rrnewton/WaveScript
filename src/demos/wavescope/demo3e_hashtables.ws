
// [2007.01.23] NOT working yet in wsc.  We don't have hash functions
// for the tuple types yet.


// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0, 44000);

s2 = iterate( w in s1 ) {
  state{ ht = hashtable(5) }
  
  // [2007.01.23] Can't recall what made my decision on this issue...
  //  ht := hashset_BANG(ht, (w.start, w.end), w.start);
  hashset_BANG(ht, (w.start, w.end), w.start);
  
  emit (w, ht);
};

s3 = iterate( (w, ht) in s2) {
  emit hashget(ht, (w.start, w.end));
}

BASE <- s3;

