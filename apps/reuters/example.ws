

include "wsqlib.ws"

syms = #["IBM", "APPL", "GOOG"]

fakestocks = iterate _ in timer(10) {
  state{ t = 0 }
  i = randomI(Array:length(syms));
  emit (SYM= syms[i], 
        TIME= t, 
        PRICE= randomI(100));
  t += randomI(10);
}


s0 = fakestocks;
s1 = SELECT(fun (r) { r.PRICE > 50 }, s0);
s2 = SELECT(fun (x) { x.SYM != "IBM" }, s1);

//s3 = WINDOW(5, s2);
//s4 = REWINDOW(4, -2, s3);


// Stonebreaker's 20 Queries:

// Query 1: send me each tick for a stock
q1 = SELECT(fun(x) { x.SYM == "IBM" }, fakestocks);

// Query 2: 30 second moving avg.
q2 = WINDOW(30, 
      SELECT(fun(x) { x.SYM == "IBM" }, fakestocks))

main = q2;
