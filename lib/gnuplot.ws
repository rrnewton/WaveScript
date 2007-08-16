

namespace Gnuplot {

  namespace Internal {

    // Doesn't add the leading "plot" command
    fun array_data(arr, fn) {
      using Array;
      //fn ("plot \"-\" using 1:2 with linespoints\n");
      for i = 0 to arr.length - 1 {
	fn (i++" "++ arr[i] ++"\n");
      };
      fn ("e\n");
    }

    fun simpleStreamOp (extracmds, plotsuffix, strm, spewdata) {
      pipe = spawnprocess("gnuplot",
			  iterate x in strm {
			    state { fst=true }
			    emit extracmds;
			    if fst 
			    then emit ("plot "   ++ plotsuffix)
			    else emit ("replot " ++ plotsuffix);
			    spewdata(x, fun(s) emit s);
			    emit ("e\n");
			  });
      merge(strm ,iterate _ in pipe {})
    }

  } // End Internal namespace.


 fun array_streamXY(extracmds, strm) {
   using Internal;
   simpleStreamOp
     (extracmds, " \"-\" using 1:2 with linespoints\n", strm,
      fun(arr,out) {
        Array:foreach(fun((x,y)) out(x++" "++y++"\n"), arr);
   	out("e\n");
      })
  }
 fun array_stream(extracmds, strm) {
   using Internal;
   simpleStreamOp
     (extracmds, " \"-\" using 1:2 with linespoints\n", strm,
      fun(arr,out) {
        Array:foreachi(fun(i,x) out(i++" "++x++"\n"), arr);
   	out("e\n");
      })
  }

 // This is a nice one, it takes a list of array streams and plots
 // them all on a single multiplot.
 fun array_stream_multiplot(extracmds, strmls) {
   ()
 }

}
