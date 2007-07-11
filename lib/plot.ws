
/* This library provides utilities for "live" plots that update as the
   data streams through.

   "cumulative" plots just grow over time with new data.  Can't run forever!
   "sliding_window" plots keep a finite amount of history

*/

Plot:cumulative1d                        :: Stream #a -> Stream #a;
Plot:cumulative2d                        :: Stream (#x * #y) -> Stream (#x * #y);

Plot:Array:cumulative1d                  :: Stream (Array #a) -> Stream (Array #a);
Plot:Array:cumulative2d                  :: Stream (Array (#x * #y)) -> Stream (Array (#x * #y));

Plot:Sigseg:cumulative1d                 :: Stream (Sigseg #a) -> Stream (Sigseg #a);
Plot:Sigseg:cumulative2d                 :: Stream (Sigseg (#x * #y)) -> Stream (Sigseg (#x * #y));


namespace Plot {


//               INTERNAL HELPER FUNCTIONS: Don't use directly                      //
// ================================================================================ //

// Takes a windowed stream.  Remembers the history and redraws the
// whole graph every time it gets a new window of data on the stream.
//
fun internal_helper(plotter, wid, get, toContainer) fun (src) {

  let cumulative = iterate( win in src) {
    state { 
      arr = Array:null;
      ind = 0;
     }
    using Array;
    //println("GrowingPlot: arr len "++ arr.length ++", ind " ++ ind);

    // If there's not enough space, we reallocate the array:
/*     if ind + win.width > arr.length */
/*     then { */
/*       arr2 = make(arr.length * 2 + win.width, win[[0]]); */
/*       for i = 0 to arr.length-1 { */
/* 	arr2[i] := arr[i]; */
/*       }; */
/*       arr := arr2; */
/*     }; */
    
    // Inefficient!! for now we just reallocate EVERY time:
    arr2 = make(arr.length + win`wid, win`get(0));
    for i = 0 to arr.length-1 {
      //println("Copying! " ++ i);
      arr2[i] := arr[i];
    };
    arr := arr2;  

    // Now we copy the new data in:
    for i = 0 to win`wid - 1 {
      arr[ind+i] := win`get(i);
    };
    ind += win`wid;

    emit arr;
  };

  // We should reproduce the original stream.
  // But we need union types for that...

  trash = plotter ( cumulative );

  /*  trash = if dims == 1 then gnuplot_array_stream  ( cumulative )
     else if dims == 2 then gnuplot_array_stream2d( cumulative )
     else wserror("unsupported dimension: "++dims);*/

  //  trash2 = iterate arr in trash {emit toSigseg(arr,0,nulltimebase) };
  // Here's a hack!  We bring it back to the same type so that we can use unionList.
  trash2 = iterate arr in trash { emit toContainer(arr) };
  merged = unionList([src, trash2]);
  iterate (i,w) in merged { if i==0 then emit w }
}

// ================================================================================ //
//                           END INTERNAL HELPER FUNCTIONS.                         //


// Sub-namespaces encapsulate different types that we work with.
// Streams of Sigsegs or Streams of Arrays:
namespace Sigseg {
  //live1d :: ArrayPlotter t -> 'foo;
  cumulative1d = Plot:internal_helper(gnuplot_array_stream,   width, fun(w,i) w[[i]], fun(x) toSigseg(x, 0, nulltimebase));
  cumulative2d = Plot:internal_helper(gnuplot_array_stream2d, width, fun(w,i) w[[i]], fun(x) toSigseg(x, 0, nulltimebase));
}

namespace Array {
  cumulative1d = Plot:internal_helper(gnuplot_array_stream,   Array:length, fun(a,i) a[i], fun(x)x);
  cumulative2d = Plot:internal_helper(gnuplot_array_stream2d, Array:length, fun(a,i) a[i], fun(x)x);
}


// These versions operate not on streams of sigsegs/arrays but streams of raw elements.

cumulative1d = Plot:internal_helper(gnuplot_array_stream,   fun(x) 1, fun(x,_) x, fun(arr) arr[0]);
cumulative2d = Plot:internal_helper(gnuplot_array_stream2d, fun(x) 1, fun(x,_) x, fun(arr) arr[0]);



}
