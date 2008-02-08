
// [2007.07.23] rrn:
// This takes 208 ms on my laptop.
// For comparison it takes only 376ms to process 1.1mb of binary data,
// do the first phase event detector, and THEN do the AML.


/*

[2008.02.07] Doing some detective work.
ACK, it looks like -O3 wasn't turned on on the C version previously!!

Here's cachegrind output from the C version.  Executable is 76K.  Bottom line: excellent locality!!

==11015== Cachegrind, an I1/D1/L2 cache profiler.
<05,1202419856.231097>main: reading testdata.txt succesfull
<05,1202419891.091695>main: farfield aml succesfull
<05,1202419891.146889>main: writing result to result.txt succesfull
==11015== 
==11015== I   refs:      1,430,539,671
==11015== I1  misses:            1,791
==11015== L2i misses:            1,387
==11015== I1  miss rate:          0.00%
==11015== L2i miss rate:          0.00%
==11015== 
==11015== D   refs:        735,148,661  (519,818,863 rd + 215,329,798 wr)
==11015== D1  misses:          173,573  (     69,711 rd +     103,862 wr)
==11015== L2d misses:            3,979  (      2,421 rd +       1,558 wr)
==11015== D1  miss rate:           0.0% (        0.0%   +         0.0%  )
==11015== L2d miss rate:           0.0% (        0.0%   +         0.0%  )
==11015== 
==11015== L2 refs:             175,364  (     71,502 rd +     103,862 wr)
==11015== L2 misses:             5,366  (      3,808 rd +       1,558 wr)
==11015== L2 miss rate:            0.0% (        0.0%   +         0.0%  )


And for wsmlton -O3.  Again, pretty excellent locality.

==11020== Cachegrind, an I1/D1/L2 cache profiler.

STARTTIMECPU: 2916

STARTTIMEREAL: 2962
Got a window on ch1, first element:~0.0442504882812
Allocating fftw plan for the first time, size 2048

ENDTIMECPU: 39890

ENDTIMEREAL: 39903
==11020== 
==11020== I   refs:      1,696,782,114
==11020== I1  misses:            8,723
==11020== L2i misses:            5,447
==11020== I1  miss rate:          0.00%
==11020== L2i miss rate:          0.00%
==11020== 
==11020== D   refs:        694,521,556  (522,566,711 rd + 171,954,845 wr)
==11020== D1  misses:          936,846  (    367,831 rd +     569,015 wr)
==11020== L2d misses:          298,912  (     19,188 rd +     279,724 wr)
==11020== D1  miss rate:           0.1% (        0.0%   +         0.3%  )
==11020== L2d miss rate:           0.0% (        0.0%   +         0.1%  )
==11020== 
==11020== L2 refs:             945,569  (    376,554 rd +     569,015 wr)
==11020== L2 misses:           304,359  (     24,635 rd +     279,724 wr)
==11020== L2 miss rate:            0.0% (        0.0%   +         0.1%  )

For reference, wsc2 is virtually the same as wsmlton above.  (Even to
the point of having a little bit of D1 misses that the raw C version
doesn't -- maybe the refcounts....)

*/




include "stdlib.ws";
include "gnuplot.ws";

// When we're not live we just print log messages to the stream.
//fun log(l,s) println(s)
fun log(l,s) {}

fun clockit(str, s) {
  s
/* to enable: this outputs a clock value:
  iterate x in s { println(str++": "++clock()); emit(x); }
*/
}

GUIENABLED = false

// Here we read in a small data sample.
samp_rate = 24000.0; 

//chans = (readFile(file, "mode: text window: 8192 rate: 24000 ") :: Stream Sigseg (Float)); 


// [2007.11.07] Disabling this because of problems with DF and amplification.

chans = 
  if true
  then { file = "testdata.txt";
         mytimer = repeater(2400, timer(20.0));
         _chans = (readFile(file, "mode: text ", mytimer) :: Stream (Float));
         __chans = window(_chans, 8192);
         repeater(21, __chans); }
  else { file = "6sec_marmot_sample.raw";
         mytimer = repeater(600,timer(80.0));
	 deep_stream_map(int16ToFloat,
	   (readFile(file, "mode: binary window: 8192", mytimer) :: Stream (Sigseg Int16))
	   	 )
       };


include "marmot2.ws";

fun run_it(chans) {
  split = deinterleaveSS(4, 2048, chans);

//ch1 = snoop("chan1 ", List:ref(split, 0)) ;
_ch1 = List:ref(split, 0);
ch2 = List:ref(split, 1);
ch3 = List:ref(split, 2);
ch4 = List:ref(split, 3);

ch1 = iterate x in _ch1 {
  state { fst = true }
  if fst then { fst:=false; println("Got a window on ch1, first element:" ++ x[[0]])};
  emit x;
};

synced0 = clockit("start",zipN_sametype(0, [ch1,ch2,ch3,ch4]));

synced1 = iterate ls in synced0 {
  using List;
  emit (ls`ref(0), ls`ref(1), ls`ref(2), ls`ref(3))
};

synced2 = iterate x in synced0 {
  print("Got synced\n");
  emit x;
};

synced = synced2;

fun SLSSmap(fn,slss)
  smap(fun (ls) List:map(fun(ss) sigseg_map(fn,ss), ls), slss);

//converted = SLSSmap(floatToInt16, synced0)

doas = oneSourceAMLTD_helper(repeater(200,synced0), 2048);
doas
}

//========================================
// Main query:

//doas = oneSourceAMLTD(synced0, 4096);

//BASE <- chans;
//BASE <- ch2;
//BASE <- ch1;
//BASE <- dewindow(ch1)
//BASE <- synced2;
//BASE <- converted;
//BASE <- gnuplot_array_stream(smap(fst, doas))
//BASE <- iterate x in doas { print("GOT FINAL RESULT\n");  emit x }

//BASE <- (smap(fst, doas))
//BASE <- smap(fun (_) (), doas)
//BASE <- timeN(20, doas)


//BASE <- run_it(chans)


BASE <- clockit("AMLdone",   
   smap(fun(_)(), timeTransformer(100, chans, run_it))
   )
