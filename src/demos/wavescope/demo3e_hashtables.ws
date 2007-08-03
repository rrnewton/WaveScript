
// [2007.01.23] NOT working yet in wsc.  We don't have hash functions
// for the tuple types yet.


// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int));

s2 = iterate( w in s1 ) {
  state{ ht = HashTable:make(5) }
  
  // [2007.01.23] Can't recall what made my decision on this issue...
  //  ht := hashset_BANG(ht, (w.start, w.end), w.start);
  HashTable:set_BANG(ht, (w.start, w.end), w.start);
  
  emit (w, ht);
};

s3 = iterate( (w, ht) in s2) {
  emit HashTable:get(ht, (w.start, w.end));
}

BASE <- s3;

