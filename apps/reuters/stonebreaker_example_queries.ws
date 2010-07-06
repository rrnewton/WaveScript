

include "wsqlib_alpha.ws"

syms = #["IBM", "GOOG", "GM", "F", "IMGN"]

lastprice = Array:make(Array:length(syms), 50.0)

//fakestocks :: Stream (| TIME : Float, SYM : String, PRICE : Float);
fakestocks = iterate _ in timer(10) {
  state{ t = 0.0 }
  i = randomI(Array:length(syms));
  // Random walk:
  lastprice[i] += (Float! (randomI(200) - 100)) / 100;
  emit (SYM= syms[i], 
        TIME= t, 
        PRICE= lastprice[i],
	VOLUME= randomI(10)
	);
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
//==============================================================================

q2a = TIMESTAMP_WINDOW((.PRICE), 30.0, 
        SELECT(id,
	       fun(x) x.SYM == "IBM",
               fakestocks))
q2 = MAP(AVG, q2a)


// Query 3:
//==============================================================================
// send me the 30 second moving average of any stock in the
// technology sector for 5 minutes, if it has risen by more than 1% in
// the last 5 minutes  

fun techsector(r) List:member(r.SYM, ["GOOG", "IBM"]);


// A predicate on a 5 min window:
fun gone_up_1percent(ss) ss[[ss.width - 1]].PRICE >= 1.01 * ss[[0]].PRICE

// This may not be efficient.  It isn't necessary to keep entire 5min
// windows to figure out if the stock has gone up.

// You really want the window to slide by time, but there's no point
// in processing the same window of tuples twice.
// The minimum unit of slide should be that which brings in a single new tuple.

tech = FILTER(techsector, fakestocks)

fivemin = fun (stocks) 
          TIMESTAMP_WINDOW_GROUPBY_MINSLIDE
           (id,       // projection
            (.SYM),   // groupby
	    5 * 60,   // window size, 5 min
            stocks)
avgs = MAP(fun(ss) (TIME= ss[[ss.width-1]].TIME, 
                    AVG= AVG_OF((.PRICE), ss),
		    SYM= ss[[0]].SYM ),
         TIMESTAMP_WINDOW_GROUPBY_MINSLIDE(id, (.SYM), 30, tech))

// filter for stocks that have gone up.
//wentup :: Stream (Sigseg (| TIME: Float,  PRICE: Float, SYM: String ));
wentup = FILTER(gone_up_1percent, fivemin(tech))

times_of_interest=  MAP(fun(ss) (TIME  = ss[[ss.width-1]].TIME, 
                                 START = ss[[0]].PRICE, 
				 END   = ss[[ss.width-1]].PRICE),
                        wentup)

joined = TIMESTAMP_JOIN(avgs, times_of_interest)

// This is optional, just an example type alias:
type Q3Result = (START:Float, END:Float, INCREASE:Float, SYM:String, AVG:Float, TIME:Float);

q3 :: Stream Q3Result;
q3 = MAP(fun((x,y)) (x | START=y.START, END=y.END, INCREASE= y.END/y.START), joined)

// Query 4:
//==============================================================================
/* send me the S&P average and the DJIA average and the percetnage
   change of each one in the last 1 minute */


// Query 5:
//==============================================================================
/* 5) send me the ticker symbol of any stock whose total volume over
   the last minute exceeds Y */

fun vol(ss) Sigseg:fold(fun(acc,r) acc + r.VOLUME, 0, ss)

Y = 90
q5 = SELECT(fun(ss) (ss[[ss.width-1]].(| SYM, TIME) | VOLUME=vol(ss)),
            fun(ss) vol(ss) > Y,
            fivemin(fakestocks))

// Query 6:
//==============================================================================
/* 6) send me the ticker symbol of any stock whose total volume over
   the last minute went up from the previous minute by more than Y% */

mins = TIMESTAMP_WINDOW_GROUPBY_MINSLIDE(id, (.SYM), 60, fakestocks)

twomins = WINDOW(2, mins)

Y2 = 1.05 // five percent

q6 = SELECT(fun(sss) { min2 = sss[[0]]; 
                       (min2[[min2.width-1]] | VOL1= vol(sss[[0]]), 
		                               VOL2= vol(sss[[1]]),
					       ~VOLUME) }, // Remove this field
            fun(sss) vol(sss[[1]]) > Y2 * vol(sss[[0]]),
            twomins)


// Query 7:
//==============================================================================
/* 7) send me the volume-weighted price of Stock Z every 30 seconds */

// Query 8:
//==============================================================================
/* 8) send me all trades of stock U whose volume is over V */

U = "IBM"
V = 7
q8 = FILTER(fun(r) (r.SYM == U && r.VOLUME > V), 
            fakestocks)

// Query 9:
//==============================================================================
/* 9) send me any pair of symbols in the technology section which both
   have a trade with volume over V in a 30 second window */

// This is a kind of join:

//q9 = MAP(fun(ls) List:map(fun (r) r.(| SYM,TIME), ls),

// TODO: this isn't really complete:
     // Finally, extract just the SYM:
q9 = MAP(fun(ls) List:map((.SYM), ls),
      // Filter for time windows with more than one over the threshold:
      FILTER(fun(ls) List:length(ls) > 1,
       // Extract all ticks over a certain volume (could push this pred 1st)
       MAP(fun(ss) List:filter(fun(r) r.VOLUME > V, ss.toArray.Array:toList),
	  // Start with 30-sec windows including ALL symbols:
          TIMESTAMP_WINDOW_GROUPBY_MINSLIDE(id, fun(_) 0, 30, fakestocks))))


// Query 10:
//==============================================================================
/* 10) send me any pair of symbols in the technology section, each of
   which has a "down tick" in a 10 second window */

// Query 11:
//==============================================================================
/* 11) send me any symbol in the technology sector with no activity in
   the last 30 seconds */

// Query 12:
//==============================================================================
/* 12) fit a polynomial curve to the last 10 trades for stock X and
   send me the parameters of the curve */

// Query 13:
//==============================================================================
/* 13) alert me if stock X goes below price Y */

// Query 14:
//==============================================================================
/* 14) alert me if stock X has risen in the last 30 seconds on a
   percentage basis more than the S&P 500 average */



//==============================================================================

main = q3

// [2009.06.04] Currently having problems with q5, q6, q8 with plain WS.
// Everything works under ws.early.

// What was the status under wsc2?  Were there WS language features
// used here not supported there?

