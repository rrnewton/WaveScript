

// 185,000 ticks per second on faith with the emulator.
// Need to increase the number of symbols though.

// [2006.12.03] Improved speed, 11.1 seconds to do 5mil ticks.

// Basically, 2.7 seconds to do 500K.  If I remove the whole body and
// just have it "emit 3" it still takes 1.5 sec to do 500K.
// That's the overhead of these lazy, thunk-heavy streams.


merged = (dataFile("ticks_splits.input", "text", 44000, 0)     :: Stream (String * Float * Int * Float))
//merged = (dataFile("ticks_splits.input", "text", 44000, 250) :: Stream (String * Float * Int * Float))
//merged = (dataFile("ticks_splits.input", "text", 44000, -1)  :: Stream (String * Float * Int * Float))

s = iterate ((sym1,t,vol,price) in merged) {
  state {
     ht = HashTable:make(300);
  }

  //sym = internString(sym1);
  sym = sym1;

  if not(HashTable:contains(ht, sym))
  then HashTable:set_BANG(ht, sym, 1.0);

  if vol == -1 // In this case price is really *factor*.
  then {
    HashTable:set_BANG(ht, sym, HashTable:get(ht,sym) *. price);
    //print("SPLIT: "++show(price)++"\n");
  }
  else {
    emit (sym,t,vol,price *. HashTable:get(ht,sym));
  };
};

BASE <- s;

