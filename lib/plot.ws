



type Plotter t = Stream t -> Stream t;

type ArrayPlotter t = Stream (Array t) -> Stream (Array t);
type SigsegPlotter t = Stream (Sigseg t) -> Stream (Sigseg t);

namespace Plot {

// Takes a windowed stream.  Remembers the history and redraws the
// whole graph every time it gets a new window of data on the stream.
//
// NOTE: this should return the original stream:
//livePlot :: ArrayPlotter t -> SigsegPlotter t;

//livePlot :: ArrayPlotter t -> Stream (Sigseg t) -> Stream (Array t);
//livePlot :: ArrayPlotter t -> 'foo;
//livePlot :: SigsegPlotter t -> 'foo;
//livePlot :: Plotter t -> Plotter t;
fun internal_helper(wid, get, toContainer) fun (src) {

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
  trash = gnuplot_array_stream(cumulative);
  //  trash2 = iterate arr in trash {emit toSigseg(arr,0,nulltimebase) };
  // Here's a hack!  We bring it back to the same type so that we can use unionList.
  trash2 = iterate arr in trash { emit toContainer(arr) };
  merged = unionList([src, trash2]);
  iterate (i,w) in merged { if i==0 then emit w }
}

namespace Sigseg {
  //live1d :: ArrayPlotter t -> 'foo;
  cumulative1d = Plot:internal_helper(width, fun(w,i) w[[i]], fun(x) toSigseg(x, 0, nulltimebase));
  cumulative2d = Plot:internal_helper(width, fun(w,i) w[[i]], fun(x) toSigseg(x, 0, nulltimebase));
}

namespace Array {
  cumulative1d = Plot:internal_helper(Array:length, fun(a,i) a[i], fun(x)x);
  cumulative2d = Plot:internal_helper(Array:length, fun(a,i) a[i], fun(x)x);
}

}
