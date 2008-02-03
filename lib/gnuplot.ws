
//include "stdlib.ws";

namespace Gnuplot {

  namespace Internal {

    fun ceiling(f) roundF(f+0.5);

    // Doesn't add the leading "plot" command
    fun array_data(arr, fn) {
      using Array;
      //fn ("plot \"-\" using 1:2 with linespoints\n");
      for i = 0 to arr.length - 1 {
	fn (i++" "++ arr[i] ++"\n");
      };
      fn ("e\n");
    }

    fun gnuplotme() {
      
    }

    // TODO: This should build up the command stream without starting the process.
    fun simpleStreamOp (extracmds, plotsuffix, strm, spewdata) {
      pipe = spawnprocess("gnuplot -persist",
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
      })
  }
 fun array_stream(extracmds, strm) {
   using Internal;
   simpleStreamOp
     (extracmds, " \"-\" using 1:2 with linespoints\n", strm,
      fun(arr,out) {
        Array:foreachi(fun(i,x) out(i++" "++x++"\n"), arr);
      })
  }

 // Automatically converts to radians based on array size.
 fun array_stream_autopolar(extracmds, strm) {
   array_streamXY("set polar;\n"++extracmds,
		 smap(fun(a) {
		   // We assume that the length of the array represents 360 degrees.
		   denom = Array:length(a)`i2f / 2.0;
	  	   Array:mapi(fun(i,mag) (i`i2f / 180.0 * const_PI, mag), a);
		 }, 
		 strm))
  }

 // This is a nice one, it takes a list of array streams and plots
 // them all on a single multiplot.
 fun array_streamXY_multiplot(startupcmds, cmdlist, strmls) {
   using List;
   //   panels = ceiling$ sqrtF(strmls.length);
   //   coords = build(panels)
   
   sizex = 0.33;
   sizey = 0.33;
   origins = List:toArray$ 
             [(0.0,  0.0),
              (0.33, 0.0),
              (0.66, 0.0),
              (0.0,  0.33),
              (0.33, 0.33),
              (0.66, 0.33),
              (0.0,  0.66),
              (0.33, 0.66)];
   extracmds = List:toArray$ cmdlist;
   hitonce = Array:make(10,false);

   pipe = spawnprocess("gnuplot -persist",
       iterate (ind,arr) in unionList(strmls) {
	 state { fst=true }
	 if fst then {
	   emit startupcmds;
	   emit 
	   "
	   set style function lines
	   set size 1.0, 1.0
	   set origin 0.0, 0.0
	   set multiplot
	   ";
	   fst := false;
	 };

/* 	 emit ("set title \"Graph "++ind++"\"\n"); */
	 emit ("set size "++sizex++","++sizey++"\n");
	 let (x,y) = origins[ind];
         emit ("set origin "++x++","++y++"\n");
         //emit "clear\n";q
         emit extracmds[ind];
         emit "clear\n";
	 //plot = if hitonce[ind] then "replot" else { hitonce[ind] := true; "plot" };
	 plot = "plot";
         emit (plot++" \"-\" using 1:2 with linespoints;\n");
         //emit "clear\n";
         //emit ("replot;\n");
         //emit ("plot \"-\" using 1:2 with linespoints;\n");
         Array:foreach(fun((x,y)) emit(x++" "++y++"\n"), arr);
         emit ("e\n");
       });
   pipe
 }


}  // End namespace


/*

"

 set title \"foo\"
 plot \"~/hello.dat\" using 1:2 with linespoints;


 #  Plot 1
 set size 0.5,0.5
 set origin 0.0,0.5
 clear
 set title \"foo2\"
 plot \"~/hello2.dat\" using 1:2 with linespoints;


 #  Plot 2
 set title \"bar\"
 set size 0.5,0.5
 set origin 0.0,0.0
 plot \"~/hello2.dat\" using 1:2 with linespoints;

 unset multiplot
 reset
"
*/
