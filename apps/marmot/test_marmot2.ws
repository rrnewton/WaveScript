
// [2007.07.23] rrn:
// This takes 208 ms on my laptop.
// For comparison it takes only 376ms to process 1.1mb of binary data,
// do the first phase event detector, and THEN do the AML.



include "stdlib.ws";
include "gnuplot.ws";

// When we're not live we just print log messages to the stream.
fun log(l,s) println(s)

GUIENABLED = false

// Here we read in a small data sample.
samp_rate = 24000.0; 

//chans = (readFile(file, "mode: text window: 8192 rate: 24000 ") :: Stream Sigseg (Float)); 


// [2007.11.07] Disabling this because of problems with DF and amplification.
/*
file = "testdata.txt"
mytimer = repeater(2400, timer(20.0))
_chans = (readFile(file, "mode: text ", mytimer) :: Stream (Float));
__chans = window(_chans, 8192);
chans = repeater(21, __chans)
*/

file = "6sec_marmot_sample.raw"
mytimer = timer(40.0)
chans = (readFile(file, "mode: binary window: 8192", mytimer) :: Stream (Sigseg Float));

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
}

synced0 = zipN_sametype(0, [ch1,ch2,ch3,ch4]);

synced1 = iterate ls in synced0 {
  using List;
  emit (ls`ref(0), ls`ref(1), ls`ref(2), ls`ref(3))
}

synced2 = iterate x in synced0 {
  print("Got synced\n");
  emit x;
}

synced = synced2;
include "marmot2.ws";

fun SLSSmap(fn,slss)
  smap(fun (ls) List:map(fun(ss) sigseg_map(fn,ss), ls), slss);

//converted = SLSSmap(floatToInt16, synced0)

//========================================
// Main query:


//doas = FarFieldDOAb(synced, sensors);
//doas = oneSourceAMLTD(synced, micgeometry, 2048); 
//doas :: Stream (Array Float);
doas = oneSourceAMLTD_helper(synced0, 2048);
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
BASE <- timeN(20, doas)

