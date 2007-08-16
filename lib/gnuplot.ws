

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

  /*
 fun array_streamXY(extracmds, strm) {
   using Internal;
   using Array;
   pipe = spawnprocess("gnuplot -persist",
		       iterate arr in strm {
			 state { fst=true }
			 emit extracmds;
			 if fst 
			 then emit ("plot \"-\" using 1:2 with linespoints\n")
			 else emit ("replot \"-\" using 1:2 with linespoints\n");
			 Array:foreach(fun((x,y)) emit (x++" "++y++"\n"), arr);
			 emit ("e\n");
		       });
   merge(strm ,iterate _ in pipe {})
 }
  */

 fun array_stream(extracmds, strm) {
   using Array;
   pipe = spawnprocess("gnuplot",
		       iterate arr in strm {
			 state { fst=true }
			 emit extracmds;
			 if fst 
			 then emit ("plot \"-\" using 1:2 with linespoints\n")
			 else emit ("replot \"-\" using 1:2 with linespoints\n");
			 for i = 0 to arr.length - 1 {
			   emit (i++" "++ arr[i] ++"\n");
			 };
			 emit ("e\n");

			 //           array(ar, fun(x) emit x)
		       });
   merge(strm ,iterate _ in pipe {})
  }

}
