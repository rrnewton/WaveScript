
include "stdlib.ws"
include "unix.ws"
include "socket.ws"

include "taq_reader.ws"

// A collection of database-like stream operators.


/****************************************************************************************************/
/* (2)  Functions called by generated WSQ code (via the C API)  [2009.10]                           */
/****************************************************************************************************/

// Interface:

// Apply windowing based on a time field.
wsq_window :: (tup -> #time, #time, Int, Stream tup) -> SS tup;
// wsq_window(extract_time, winsize, slide, strm)
// For now SLIDE is in terms of number of TUPLES.
// In the future it may be possible to specify minimum slide in the original time unit.



//wsq_join_leftonly :: (a -> time, Stream a, Stream a) -> Stream (a * a);

// wsq_join_helper    :: (a -> b, Stream a, Stream a, (a, a) -> c) -> Stream c;
//wsq_join_leftonly  :: (a -> time, Stream a, Stream a) -> Stream a;



 /* wsq_reuterSource                         :: (Float, String) -> Stream DummySchema99; */
 /* wsq_window                               :: (a -> #b, #b, Int, Stream a) -> SS a; */
 /* wsq_window_super                         :: (a, b, c, Stream d) -> Stream #e; */
 /* wsq_windowJoin                           :: (a, b, Stream c, Stream d, e) -> Stream f; */
 /* wsq_mergeMonotonic                       :: (a, b, c) -> d; */
 /* wsq_printer                              :: (a, Stream b) -> Stream c; */
 /* wsq_connect_out                          :: (a, Uint16, Stream b) -> Stream c; */
 /* wsq_connect_in                           :: (String, Uint16) -> Stream a; */
 /* wsq_SUM                                  :: (a -> #b, Sigseg a) -> #b; */
 /* wsq_MIN                                  :: (a -> b, Sigseg a) -> b; */
 /* wsq_MAX                                  :: (a -> b, Sigseg a) -> b; */
 /* wsq_AVG                                  :: (a -> #b, Sigseg c) -> #d; */
 /* wsq_FIRST                                :: (a -> b, Sigseg a) -> b; */
 /* wsq_LAST                                 :: (a -> b, Sigseg a) -> b; */


wsq_asciiFileSource :: (Float, String, String) -> Stream TAQ_Tup;
fun wsq_asciiFileSource(rate, schema, datfile) {
  read_TAQ_ASCII_tuples(rate, fun() datfile)
}

//====================================================================================================

fun discard(s) iterate _ in s { }


all_syms = #["IBM", "GOOG", "GM", "F", "IMGN", "MSFT",
             // Supplementing this with a bunch of other symbols:
             "AAPL", "AAUKY", "AAV", "AAWW", "AB", "ABAX", "ABB", "ABC", "ABFS", "ABG", "ABM", "ABMD", "ABT", "ABV", "ABVT", "ABX",
             "ACC", "ACCL", "ACE", "ACET", "ACF", "ACGL", "ACGY", "ACH", "ACI", "ACIW", "ACL", "ACLI", "ACM", "ACN", "ACO", "ACOM",
             "ACOR", "ACTG", "ACV", "ACXM", "ADBE", "ADCT", "ADI", "ADM", "ADP", "ADRE", "ADS", "ADSK", "ADTN", "ADVS", "ADY",
             "AEE", "AEG", "AEIS", "AEL", "AEM", "AEO", "AEP", "AER", "AES", "AET", "AEZ", "AF", "AFAM", "AFFX", "AFFY", "AFG", "AFL",
             "AFSI", "AGAM", "AGCO", "AGG", "AGII", "AGL", "AGM", "AGN", "AGNC", "AGO", "AGP", "AGQ", "AGU", "AGYS", "AHGP", "AHL",
             "AHS", "AHT", "AIG", "AIMC", "AIN", "AINV", "AIPC", "AIR", "AIRM", "AIT", "AIV", "AIXG", "AIZ", "AJG", "AKAM", "AKR",
             "AKS", "ALB", "ALE", "ALEX", "ALGN", "ALGT", "ALJ", "ALK", "ALKS", "ALL", "ALNY", "ALOG", "ALSK", "ALTE", "ALTH",
             "ALTR", "ALV", "ALXN", "AM", "AMAG", "AMAT", "AMB", "AMCC", "AMD", "AME", "AMED", "AMG", "AMGN", "AMJ", "AMKR",
             "AMLN", "AMMD", "AMN", "AMP", "AMR", "AMRI", "AMSC", "AMSF", "AMSG", "AMT", "AMTD", "AMX", "AMZN", "AN", "ANDE",
             "ANF", "ANGO", "ANH", "ANN", "ANR", "ANSS", "ANV", "ANW", "AOL", "AON", "AONE", "AOS", "APA", "APAC", "APC", "APD",
             "APEI", "APH", "APKT", "APL", "APOG", "APOL", "APSG", "APU", "APWR", "ARAY", "ARB", "ARBA", "ARCC", "ARD", "ARE",
             "ARG", "ARGN", "ARI", "ARII", "ARJ", "ARLP", "ARM", "ARMH", "ARO", "ARP", "ARRS", "ARST", "ART", "ARUN", "ARW", "ASA",
             "ASBC", "ASCA", "ASEI", "ASF", "ASFI", "ASH", "ASIA", "ASMI", "ASML", "ASPS", "ASTE", "ATAC", "ATHN", "ATHR",
             "ATI", "ATK", "ATLS", "ATMI", "ATNI", "ATO", "ATPG", "ATR", "ATU", "ATVI", "ATW", "AU", "AUO", "AUXL", "AUY", "AVA",
             "AVAV", "AVB", "AVD", "AVGO", "AVID", "AVP", "AVT", "AVTR", "AVY", "AWC", "AWH", "AWI", "AWK", "AXAHY", "AXE", "AXL",
             "AXP", "AXS", "AYE", "AYI", "AYR", "AZN", "AZO", "AZSEY"
             ];

// This is a temporary fake data source with a temporary fake schema.
type DummySchema99 = (| SYM:String, TIME:Float, PRICE:Float, VOLUME:Int );
wsq_randomSource :: (Float, String) -> Stream DummySchema99;
fun wsq_randomSource(freq, schema) {

  Unix:puts_err("<WSQ> Creating dummy stocktick source streaming " ++ Array:length(all_syms) ++ " different symbols.\n"); 

  lastprice = Array:make(Array:length(all_syms), 50.0);
  iterate _ in timer(freq) {
    state{ t = 0.0 }
    i = randomI(Array:length(all_syms));
    // Random walk, change arithmetically:
    lastprice[i] += (Float! (randomI(200) - 100)) / 100;
    // Here is a geometric one instead:
    //lastprice[i] *= (Float! (randomI(50) + 75)) / 100;
    if lastprice[i] < 0
    then lastprice[i] := 0;

    emit (SYM= all_syms[i], 
          TIME= t, 
          PRICE= lastprice[i],
	      VOLUME= randomI(10) + 1
  	      );
    t += randomI(10).gint;
  }
}

// NON-Random version.  
wsq_nonRandomSource :: (Float, String) -> Stream DummySchema99;
fun wsq_nonRandomSource(freq, schema) {
  Unix:puts_err ("<WSQ> Creating (nonrandom) dummy stocktick source streaming " ++ Array:length(all_syms) ++ " different symbols.\n"); 
  iterate _ in timer(freq) {
    state{ i = 0 }
    i += 1;
    ind = moduloI(i, Array:length(all_syms));

    f = Float! i;
    emit (SYM= all_syms[ind], 
          TIME= f,
          PRICE= f,
	      VOLUME= i
  	      );
  }
}




wsq_filter  = stream_filter;
wsq_project = stream_map;


//====================================================================================================
// Windowing:

// UNFINISHED:

// This produces windows of a non-fixed size based on qualities of the data.
// It can be used for time-stamp based windows but is more general than that.
/*
fun wsq_window_general (pred, strm) {
  using Array;
  arbitrary_start_size = 16;

  //fun count() { dif = tl - hd; if dif < 0 then dif + arr`length else dif; };

  iterate(new in strm) {
    state{ 
      // The buffer for accumulating elements:
      arr = makeUNSAFE(arbitrary_start_size);
      hd = 0;    // Head index into the circular buffer.
      tl = 0;    // Tail index into the circular buffer.
      count = 0; // How many elements are in the buffer.

      startsamp = 0`gint;
    }

    curlen = arr`length;

    // Prune old elements that cause the predicate to fail.
    // (This could remove ALL saved elements.)
    while not( pred(arr[hd], new)) && count > 0 // hd < tl 
    {
      hd += 1;
      count -= 1;
      if hd == curlen then hd := 0;
    }

    // Add the new element, if there's not enough space, double the buffer:    
    if count == curlen then {
       old = arr; 
       arr := makeUNSAFE(2 * count);
       // Copy the first part:
       blit(arr, 0, old, hd, count - hd);
       // Copy the part after the wrap-around:
       if tl < hd then 
          blit(arr, 0, old, _, count - hd);
       hd := 0;
       tl := count;
    };

    // Ok, NOW we are ready to add it for real:
    arr[tl] = new;
    tl += 1;
    count += 1;
    if tl == curlen then tl := 0;

    // UNFINSHED:
    // How far to slide is a totally different question and hard to answer in as general a way.       
    //emit toSigseg(arr, startsamp, nulltimebase);
  }
}
*/

// This version is somewhat less ambitious it provides data-dependent
// windowing but requires that windows be defined in terms of a scalar
// "time" property.

/* One tradeoff here is whether or not windows should WAIT until they
   fill up, or if they should fire immediately.  In the latter case
   there's really no guarantee as to the size of the actual windows. 
   Of course, that's always the case with very sparse datapoints.  */
fun wsq_window(extract_time, winsize, slide, strm) {
  using Array;
  arbitrary_start_size = 16;
  iterate(new in strm) {
    state{ 
      // The buffer for accumulating elements:
      arr = makeUNSAFE(arbitrary_start_size);
      hd = 0;    // Head index into the circular buffer.
      tl = 0;    // Tail index into the circular buffer.
      count = 0; // How many elements are in the buffer.
      slide_counter = slide;  // Count down to producing an output.
    }

    newtime = extract_time(new);
    curlen = arr`length;

    // Prune old elements that cause the predicate to fail.
    // (This could remove ALL saved elements.)
    while (newtime - extract_time(arr[hd])) > winsize  && count > 0 
    {
      hd += 1;
      count -= 1;
      if hd == curlen then hd := 0;
    }

    // Add the new element, if there's not enough space, double the buffer:    
    if count == curlen then {
       old = arr; 
       arr := makeUNSAFE(2 * count);
       // Copy the first part:
       blit(arr, 0, old, hd, (if tl < hd then curlen else tl) - hd); 
       // Copy the part after the wrap-around:
       if tl < hd then blit(arr, 0, old, 0, count - hd);
       hd := 0;
       tl := count;
    };

    // Ok, NOW we are ready to add it for real:
    newlen = arr`length;
    arr[tl] := new;
    tl += 1;
    count += 1;
    if tl == newlen then tl := 0;

    // INEFFICIENT FIXME -- currently with a slide of 1 this will exhibit NO SHARING.
    // The problem is that we don't want to introduce latency....
    // A solution would be to unsafely modify sigsegs after they have left this kernel...

    // TODO: support sliding in meaningful time units:
    slide_counter -= 1;    
    if slide_counter == 0 then {

       // For now we just COPY like mad:
       // Pointless to have a circular buffer if we do this much copy.

       // TODO!! Ensure that count != 0..

       tmparr = makeUNSAFE(count);

       blit(tmparr, 0, arr, hd, (if tl < hd then newlen else tl) - hd); 
       if tl < hd then blit(tmparr, 0, arr, 0, count - hd);
       
       emit toSigseg(tmparr, (Int64! extract_time(tmparr[0])), nulltimebase);

       slide_counter := slide;
    };

    // UNFINSHED:
    // How far to slide is a totally different question and hard to answer in as general a way.       
    //
  }
}

// This version implements the memory sharing strategy.
// UNFINISHED:
fun wsq_window_super(extract_time, winsize, slide, strm) {
  using Array;
  arbitrary_start_size = 16;
  iterate(new in strm) {
    state { 
      // Double buffer:
      buf1 = null;                              // The old one.
      buf2 = makeUNSAFE(arbitrary_start_size);  // The current one.

      // We "remove" elements from the front of the old buffer:
      hd1 = 0;
      // And we add them to the tail of the new buffer.
      tl2 = 0;

      slide_counter = slide;  // Count down to producing an output.
    }

    // If we get rid of the circular approach and never overwrite
    // array positions we could get nice sharing here..
    // Would need to be able to point a sigseg at *part* of an array.

    // Point to the current array and join them:
    // sig1 = unsafeToSigseg(arr_old, hd, len?, startsamp, nulltimebase);
    // sig2 = ...
    //  joinsegs
    
    emit 99;
  }
}

//====================================================================================================
// Joins and Merges

// If we ONLY care about aggregating windows, we do not even need to allocate.
// fun wsq_window_aggronly
// We can do as CQL and produce only add/remove tuples (deltas).


// (app wsq_window (lambda (,fst ,lst) ,predicate) ,(edge-sym in))


fun wsq_windowJoin(cmpr, combine, left, right, winsize) {
  iterate x in union2(left,right) {
    error("WindowJoin operator unfinished!!");
    //    print(" -*|*- Running window joiner: \n")

 // GETTING A TYPE CHECKING ERROR!!!!
/*
    case x { 
      Left(t):  print(" -*|*- Running window joiner: LEFT\n")
      Right(t): print(" -*|*- Running window joiner: RIGHT\n")
    }
*/
    //println(" -*|*- Running window joiner: " ++ x);
    

  }
}

// Requires streams to be the same type:
fun wsq_mergeMonotonic(extractor, s1, s2) {
  using FIFO;
  iterate(x in union2(s1,s2)) {
    state {
      buf1 = make(10); // ARBITRARY -- FIFO sizes
      buf2 = make(10);
    }
    case x { 
      Left  (a): buf1.enqueue(a)
      Right (b): buf2.enqueue(b)
    };
    while (buf1.elements > 0 && buf2.elements > 0) {
      a = buf1.peek(0);      
      b = buf2.peek(0);      
      if a.extractor <= b.extractor  then {
        buf1.dequeue();
        emit a;
      } else {
        buf2.dequeue();
        emit b;
      }
    }
  }
}

fun wsq_join_helper(extractor, s1, s2, pickresult) {
  using FIFO;
  iterate(x in union2(s1,s2)) {
    state {
      buf1 = make(10);
      buf2 = make(10);
    }

    case x { 
      Left  (a): buf1.enqueue(a)
      Right (b): buf2.enqueue(b)
    };

    //buf1.peek(0).FOOBAR + 1;

    while (buf1.elements > 0 && buf2.elements > 0) {

      a = buf1.peek(0);      
      b = buf2.peek(0);      
      a_time = extractor(a);
      b_time = extractor(b);
      if (a_time == b_time) then {
      //if (ismatch(a,b)) then  {

        buf1.dequeue();
        buf2.dequeue();
        emit pickresult(a, b);
      } else {
        // The younger one is necessarily trash, because of monotonicity 
	//tm1 = a.TIME;
	//tm2 = b.TIME;
	//if tm1 < tm2 then buf1.dequeue() else ();
	// SUBTLE AND INTERESTING BUG!!  If forces the values to be the same here.
        if a_time < b_time
        then { buf1.dequeue(); () }
        else { buf2.dequeue(); () }
      }
    }
  }
}

// Find matches (inner join) but then output only the left tuple.
fun wsq_join_leftonly(extractor, s1, s2) {
    wsq_join_helper(extractor, s1,s2, fun (l,r) l);
}


//====================================================================================================

fun wsq_printer(str, s) {
  //stream_map(fun(x) { print(x); x }, s)
  iterate x in s { 
   state {
     do_flush = 
       if GETENV("WSQ_NOFLUSH") == "" 
       then true
       else { Unix:puts_err(" <WSQ> DISABLING flushing after printing stream elements.  Be careful.\n");
              false; };

      do_print = 
       if GETENV("WSQ_NOPRINT") == "" 
       then true
       else { Unix:puts_err(" <WSQ> DISABLING ALL PRINTING.  BE WARNED!\n");
              false; };

     stdout :: FileDescr = ((foreign("ws_get_stdout", ["stdio.h"]) :: () -> FileDescr))();  
   }
   if do_print then 
   {
      print(str);
      print(x); 
      print("\n");    
      // [2010.12.08] What made me do a flush? 
      // [2011.02.25] Reenabling... could do something better here and catch kill signals.
      if do_flush then Unix:fflush(stdout); 
   }
  }
}

/*  Handling network communication */

//====================================================================================================

fun wsq_connect_out(host, prt, strm) 
{
  Unix:puts_err(" <WSQ> Connection operator for outgoing (server) socket, port: " ++ prt ++ "\n");
  socket_out(strm, prt);
}

fun wsq_connect_in(host, prt) 
{
  Unix:puts_err(" <WSQ> Creating incoming (client) socket, host "++ host ++" port: " ++ prt ++ "\n");
  socket_in(host, prt)
}


//====================================================================================================
// AGGREGATION

wsq_SUM        :: (a -> #b, Sigseg a) -> #b;
wsq_MIN        :: (a -> b,  Sigseg a) -> b;
wsq_MAX        :: (a -> b,  Sigseg a) -> b;
wsq_AVG        :: (a -> #b, Sigseg c) -> #b;
wsq_FIRST      :: (a -> b,  Sigseg a) -> b;
wsq_LAST       :: (a -> b,  Sigseg a) -> b;
wsq_FIRSTWHERE :: (a -> b, a -> Bool, Sigseg a) -> b;
wsq_LASTWHERE  :: (a -> b, a -> Bool, Sigseg a) -> b;


// (1) Simple version, windowed streams pass sigsegs, aggregators operate on sigsegs:

fun wsq_SUM(extract, ss) Sigseg:fold(fun (acc,x) acc + extract(x), 0, ss);

// FIXME: these two implementations do an extra comparison:
fun wsq_MIN(extract, ss) Sigseg:fold(fun (acc,x) min(acc, extract(x)), extract(ss[[0]]), ss);
fun wsq_MAX(extract, ss) Sigseg:fold(fun (acc,x) max(acc, extract(x)), extract(ss[[0]]), ss);


// BUG: [2010.07.06] Infers too weak a type:
// wsq_AVG        :: (a -> #b, Sigseg c) -> #d;
fun wsq_AVG(extract, ss) {
    fun folder((num, cnt), x) (num + extract(x), cnt+1);
    let (num,den) = Sigseg:fold(folder, (0,0), ss); 
    return (num / den);
}

// These are not incrementally computable, require a memory of the whole window:
fun wsq_FIRST(extract, ss) extract(ss[[0]]);
fun wsq_LAST (extract, ss) extract(ss[[ss`width - 1]]);

fun wsq_FIRSTWHERE(extract, predicate, ss) {
    i = 0;
    while not( predicate(ss[[i]]) ) {                            
      i += 1;
    };  
    return extract( ss[[i]] );
}

fun wsq_LASTWHERE(extract, predicate, ss) {
   // Inefficient currently:
   i = ss`width - 1;
   while not( predicate(ss[[i]]) ) {                            
      i -= 1;
   };  
   return extract( ss[[i]] );
}



// (2) Alternate version: +/- add/remove tuples as in CQL.
// This is probable more efficient but doesn't conveniently support
// aggregates that are not incrementally computable (that need the
// whole window).
