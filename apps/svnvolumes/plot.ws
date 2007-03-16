

// NOTE: this should return the original stream:

// Takes a windowed stream.  Remembers the history and redraws the
// whole graph every time it gets a new window of data on the stream.
livePlot :: Stream (Sigseg t) -> Stream (Array t);
fun livePlot (S) {
  gnuplot_array_stream(
   iterate( win in S ) {
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
    arr2 = make(arr.length + win.width, win[[0]]);
    for i = 0 to arr.length-1 {
      //println("Copying! " ++ i);
      arr2[i] := arr[i];
    };
    arr := arr2;  

    // Now we copy the new data in:
    for i = 0 to win.width-1 {
      arr[ind+i] := win[[i]];
    };
    ind += win.width;

    emit arr;
  })
}
