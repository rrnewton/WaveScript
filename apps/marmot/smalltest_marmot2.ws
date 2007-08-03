
// [2007.07.23] rrn:
// This takes 208 ms on my laptop.
// For comparison it takes only 376ms to process 1.1mb of binary data,
// do the first phase event detector, and THEN do the AML.



include "stdlib.ws";

//include "marmot_first_phase.ws";

// Here we read in a small data sample.
file = "testdata.txt"
//chans = (readFile(file, "mode: text window: 8192 rate: 24000 ") :: Stream Sigseg (Float)); 
_chans = (readFile(file, "mode: text ", timer(2400.0)) :: Stream (Float));
chans = window(_chans, 8192);

split = deinterleaveSS(4, 2048, chans);

ch1 = List:ref(split, 0);
ch2 = List:ref(split, 1);
ch3 = List:ref(split, 2);
ch4 = List:ref(split, 3);

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

//========================================
// Main query:

//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced, sensors, 2048); 

//BASE <- gnuplot_array_stream(doas)
BASE <- iterate x in doas { print("GOT FINAL RESULT\n");  emit x }
//BASE <- synced;
//BASE <- dewindow(ch1)
