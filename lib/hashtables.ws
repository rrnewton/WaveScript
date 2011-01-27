

// A simple implementation of hashtables in the language (assuming a
// polymorphic hash function).

// Here is the interface expsosed:

HashTable:make :: Int -> HashTable (key,val);
HashTable:get  :: (HashTable (key,val), key) -> val;

//HashTable:tryget :: (HashTable (key,val), key, val) -> val;

HashTable:set_BANG :: (HashTable (key,val), key, val) -> ();
//HashTable:set_pure :: (HashTable (key,val), key, val) -> HashTable (key,val);
//HashTable:set :: (a,b,c) -> d;

HashTable:foreach :: ( (key,val) -> (), HashTable(key,val)) -> ();

// TODO: When we're sure of sum-type support across all backends, make this a unary sum.
// Presently this uses a one-element array to store the number of elements:
// NEED FIRST CLASS REFS!!!
type HashTable(key,val) = (Array Int * Array(Array (List (key * val))));

namespace HashTable {
  using Internal; // For 'hash'

  primes = 
    #[
      53,         97,         193,       389,       769,
      1543,       3079,       6151,      12289,     24593,
      49157,      98317,      196613,    393241,    786433,
      1572869,    3145739,    6291469,   12582917,  25165843,
      50331653,   100663319,  201326611, 402653189, 805306457, 
      1610612741, 3221225473, 4294967291
     ];
  
  fun fst((a,b)) a;
  fun snd((a,b)) b;

  fun make(size_hint) {
    (#[0], #[Array:make(size_hint, [])])
  }

  // Takes only the buckets, not the whole data structure.
  fun resize(buckets) {
    using Array;
    //print("Resizing to "++ 2 * buckets.length + 1 ++"\n");
    // TODO curb it by max array size:
    newb = make(2 * buckets.length + 1, []);

    for i = 0 to buckets.length-1 {
      ptr = buckets[i];
      while not(List:isNull(ptr)) {
        hsh = absI(hash(ptr.head.fst));
	bucket = hsh.moduloI(newb.length);
	newb[bucket] := ptr.head ::: newb[bucket];
	ptr := ptr.tail;
      }      
    };
    newb
  }

  // Replaces if it's there:
  fun set_BANG((sz,oright), key, val) {
    ht = oright[0];
    hsh = absI(hash(key));
    using Array;
    bucket = hsh.moduloI(ht.length);
    ls = ht[bucket];
    using List;
    acc = [];
    // Now search for the object:
    while (not( ls.isNull ) && ls.head.fst != key) {
      acc := ls.head ::: acc;
      ls := ls.tail;
    };
    if ls.isNull 
    then {    
      ht[bucket] := (key,val) ::: ht[bucket];
      sz[0] += 1;      
      if sz[0] > Array:length(ht) then oright[0] := resize(ht); // NEED TO MUTATE THE BUCKETS
    }
    else ht[bucket] := (key,val) ::: append(acc, ls.tail);
  }


  // If we added set-car! to the language that would make this better:
  fun get((sz,oright), key) {
    ht = oright[0];
    hsh = absI(hash(key));
    using Array;
    bucket = hsh.moduloI(ht.length);
    ls = ht[bucket];
    using List;
    // Now search for the object:
    while (not( ls.isNull ) && ls.head.fst != key) {
      ls := ls.tail;
    };
    if ls.isNull then error("HashTable:get could not find key "++ key);
    ls.head.snd; // Return
  }

  fun contains((sz,oright), key) {
    ht = oright[0];
    hsh = absI(hash(key));             using Array;
    bucket = hsh.moduloI(ht.length);
    ls = ht[bucket];                   using List;
    // Now search for the object:
    while (not( ls.isNull ) && ls.head.fst != key) {
      ls := ls.tail;
    };
    ls.isNull.not
  }

  fun foreach(fn, (sz,dat)) {
    using Array;
    buckets = dat[0];
    for i = 0 to buckets.length-1 {
      ptr = buckets[i];
      while not(List:isNull(ptr)) {
	let (k,v) = ptr.head;
	fn(k,v);
	ptr := ptr.tail;
      }
    }
  }
  
  fun rem_BANG((sz,oright), key) {
    ht = oright[0];
    hsh = absI(hash(key));              using Array;
    bucket = hsh.moduloI(ht.length);    using List;
    ls = ht[bucket];
    acc = [];
    // Now search for the object:
    while (not( ls.isNull ) && ls.head.fst != key) {
      acc := ls.head ::: acc;
      ls := ls.tail;
    };
    if ls.isNull then error("HashTable:rem_BANG could not find key "++ key);
    ht[bucket] := append(acc, ls.tail);
  }

  // Deprecated:
  fun set_pure((sz,dat), key,val) {
    using Array;
    arr = dat[0];
    newarr = Array:makeUNSAFE(arr.length);
    Array:blit(newarr, 0, arr,0, arr.length);
    new = (#[ sz[0] ], #[ newarr ]);
    new.set_BANG(key,val);
    new;
  }
  // Deprecated:
  fun rem_pure((sz,dat), key) {
    using Array;
    arr = dat[0];
    newarr = Array:makeUNSAFE(arr.length);
    Array:blit(newarr, 0, arr,0, arr.length);
    new = (#[ sz[0] ], #[ newarr ]);
    new.rem_BANG(key);
    new;
  }

  set = set_BANG;
  rem = rem_BANG;
};


