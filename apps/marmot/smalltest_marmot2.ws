

include "marmot2.ws";


// Here we read in a small data sample.
file = "testdata.txt"

chans = (readFile(file, "mode: text window: 8192 rate: 24000 ") :: Stream Sigseg (Float)); 

split = deinterleaveSS(4, 2048, chans);

ch1 = List:ref(split, 0);
ch2 = List:ref(split, 1);
ch3 = List:ref(split, 2);
ch4 = List:ref(split, 3);

synced0 = zipN_sametype(0, [ch1,ch2,ch3,ch4]);

synced = iterate ls in synced0 {
  using List;
  emit (ls`ref(0), ls`ref(1), ls`ref(2), ls`ref(3))
}

//========================================
// Main query:

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced0, sensors, 4096); 

//BASE <- gnuplot_array_stream(doas)
BASE <- doas
//BASE <- synced;
