

include "wsqlib.ws"

syms = #["IBM", "APPL", "GOOG"]

fakestocks = iterate _ in timer(10) {
  state{ t = 0.0l }
  i = randomI(Array:length(syms));
  emit (SYM= syms[i], 
        TIME= t, 
        PRICE= randomI(100));
  t += randomI(10).gint;
}


s0 = fakestocks;
s1 = SELECT(fun (r) { r.PRICE > 50 }, s0);
s2 = SELECT(fun (x) { x.SYM != "IBM" }, s1);

//s3 = WINDOW(5, s2);
//s4 = REWINDOW(4, -2, s3);


// Stonebreaker's 20 Queries:

// Query 1: send me each tick for a stock

// x.(PRICE, SYM)

q1 = SELECT(  fun(x) { (| PRICE=x.PRICE, SYM=x.SYM ) }, 
  /* FROM */  fakestocks, 
  /* WHERE */ fun(x) { x.SYM == "IBM" });

// Query 2: 30 second moving avg.
/*
q2 = MAP(AVG,      
      WINDOW(30.0l, 
       SELECT(fun(x) { x.SYM == "IBM" }, fakestocks))
  )
*/


// 3) send me the 30 second moving average of any stock in the
// technology sector for 5 minutes, if it has risen by more than 1% in
// the last 5 minutes  
//q3 = WINDOW(30 

main = q1;
