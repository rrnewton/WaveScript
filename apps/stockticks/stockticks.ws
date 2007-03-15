

// 185,000 ticks per second on faith with the emulator.
// Need to increase the number of symbols though.

// [2006.12.03] Improved speed, 11.1 seconds to do 5mil ticks.

// Basically, 2.7 seconds to do 500K.  If I remove the whole body and
// just have it "emit 3" it still takes 1.5 sec to do 500K.
// That's the overhead of these lazy, thunk-heavy streams.

//merged = stockStream();

//merged = dataFile("ticks_splits.input","text",True); merged =
//(dataFile("ticks_splits.input", "text", 250) :: Signal
//(String,Float,Int,Float))
merged = (dataFile("ticks_splits.input", "text", -1, 44000) 
	  :: Stream (String * Float * Int * Float))


s = iterate ((sym,t,vol,price) in merged) {
  state {
     ht = hashtable(300);
  }

  if not(hashcontains(ht, sym))
  then hashset_BANG(ht, sym, 1.0);

  if vol == -1 // In this case price is really *factor*.
  then {
    hashset_BANG(ht, sym, hashget(ht,sym) *. price);
    //print("SPLIT: "++show(price)++"\n");
  }
  else {
    emit (sym,t,vol,price *. hashget(ht,sym));
  };
};

BASE <- s;

