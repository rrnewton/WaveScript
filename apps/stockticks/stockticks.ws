

// 185,000 ticks per second on faith with the emulator.
// Need to increase the number of symbols though.

// [2006.12.03] Improved speed, 11.1 seconds to do 5mil ticks.

// Basically, 2.7 seconds to do 500K.  If I remove the whole body and
// just have it "emit 3" it still takes 1.5 sec to do 500K.
// That's the overhead of these lazy, thunk-heavy streams.

include "stdlib.ws";


// [2007.11.01]
// This tries to isolate a particular component of the graph for
// timing.  It's pretty limited right now.  It batches up some tuples
// using holdAndRepeat, but it prints the ENDTIME for every single
// output tuple on the way out.  So you have to take the last message.
fun timeTransformer_obsolete(num, reps, input, strans) {
  fun strtfn() { 
    print("STARTTIME:"); print(show(clock())); print("\n") 
  };
  fun endfn(x) { print("ENDTIME: "); print(show(clock())); print("\n"); x };
  output = strans $ holdAndRepeat(num, reps, strtfn, input);
  smap(endfn, output);
}


fun processticks(merged)
 iterate ((sym1,t,vol,price) in merged) {
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


merged = (readFile("ticks_splits.input", "mode: text", timer(44000.0))
          :: Stream (String * Float * Int * Float))
//merged = (dataFile("ticks_splits.input", "text", 44000, 250) :: Stream (String * Float * Int * Float))
//merged = (dataFile("ticks_splits.input", "text", 44000, -1)  :: Stream (String * Float * Int * Float))


// Sparsify to not do too much printing.
//BASE <- sparsify(100,s);
/* BASE <- smap(fun (x) { println("ENDTIME: "++clock()); x },  */
/*          sparsify(100 * 1000,  */
/* 	   repeater(1000, 2000,  */
/* 		    fun() println("STARTTIME: "++ clock()), s))) */


// Run any of these for 9 tuples.
measurement1 = 
        timeTransformer_obsolete(1000, 1000, merged,
          fun(s) sparsify(100 * 1000,  processticks(s)))
measurement2 = 
        timeTransformer(9,
             holdAndRepeat(1000, 1000, fun() (), merged),
	     fun(s) sparsify(100 * 1000,  processticks(s)))
measurement3 = 
       sparsify(100 * 1000, 
         timeTransformer(900 * 1000,
  	   holdAndRepeat(1000, 1000, fun() (), merged), processticks))


BASE <- measurement2
