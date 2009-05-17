

include "wsqlib.ws"

syms = #["IBM", "APPL", "GOOG", "GM"]

lastprice = Array:make(Array:length(syms), 50.0)

fakestocks :: Stream (| TIME : Float, SYM : String, PRICE : Float);
fakestocks = iterate _ in timer(10) {
  state{ t = 0.0 }
  i = randomI(Array:length(syms));
  // Random walk:
  lastprice[i] += (Float! (randomI(200) - 100)) / 100;
  emit (SYM= syms[i], 
        TIME= t, 
        PRICE= lastprice[i]);
  t += randomI(10).gint;
}


//s0 = fakestocks;
//s1 = SELECT(fun (r) { r.PRICE > 50 }, s0);
//s2 = SELECT(fun (x) { x.SYM != "IBM" }, s1);
//s3 = WINDOW(5, s2);
//s4 = REWINDOW(4, -2, s3);


// Identity function:
fun id(x) x


// Stonebreaker's 20 Queries:
//================================================================================

// Query 1: send me each tick for a stock

// Note that it is clumsy to write a new record like this:
//    (| PRICE=x.PRICE, SYM=x.SYM )

// The shorthand is:
//    x.(| PRICE,SYM )


// This is a shorter, but equivalent, syntax:
q1 = SELECT(  fun(x) x.(| PRICE, SYM ) , 
  /* WHERE */ fun(x) x.SYM == "IBM",
  /* FROM  */ fakestocks);


// Query 2: 30 second moving avg.

q2a = TIMESTAMP_WINDOW(fun(x) x.PRICE, 30.0, 
        SELECT(id,
	       fun(x) x.SYM == "IBM",
               fakestocks))
q2 = MAP(AVG, q2a)


// Query 3) 
// send me the 30 second moving average of any stock in the
// technology sector for 5 minutes, if it has risen by more than 1% in
// the last 5 minutes  

fun techsector(r) List:member(r.SYM, ["GOOG", "IBM"]);
//fun techsector(r) List:member(r.SYM, ["IBM"]);


// A predicate on a 5 min window:
fun gone_up_1percent(ss) ss[[ss.width - 1]].PRICE >= 1.01 * ss[[0]].PRICE

// This may not be efficient.  It isn't necessary to keep entire 5min
// windows to figure out if the stock has gone up.

grouped = TIMESTAMP_WINDOW_GROUPBY2(id,           // projection
                                   fun(r) r.SYM,  // groupby
                                   5*60,          // window size, 5 min
           FILTER(techsector, fakestocks))

// You really want the window to slide by time, but there's no point
// in processing the same window of tuples twice.
// The minimum unit of slide should be 


// FIXME: We don't actually know the desired window size...
// INSTEAD WE JUST WANT TO SLIDE BY N! 
//sliding = REWINDOW_GROUPBY(fun(ss) ss[[0]].SYM, 20, 0, grouped)

// filter for stocks that have gone up.
//wentup = FILTER(gone_up_1percent, sliding)

//joined = TIMESTAMP_ZIP(averaged, wentup)
// MAP(fun((x,y)) x, joined)

//q3 = wentup



//main = fakestocks

//main = MAP(fun(ss) Sigseg:map(fun(r) r.PRICE, ss), q3);
//main = MAP(fun(r) r.TIME, q3.dewindow);
//main = q3

//main = MAP(fun(ss) (ss.start, ss[[0]].SYM), q3);


//main = REWINDOW_GROUPBY( (.SYM) q3, 10, -5);



// main = //MAP(fun (ss) toArray$ Sigseg:map(fun(r) r.PRICE, ss), 
          //)
//main = rewindow(q3, 4, -2);

main = grouped
//     main = window(timer(3), 10)
