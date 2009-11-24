

// A little testing on valor:
//   6.3 sec static elab single threaded
//   6.7 sec w/ par turned on (1 thread)
// Oops, that was with hashtable-based regiment-primitive?
//   9.65 sec static elab single threaded
//   9.64 sec w/ par turned on (1 thread)
//   10.4 sec w/ 2 threads
//   12.0 sec w/ 3 threads
// ... sec w/ 8 threads
// Currently it gets a regiment-primitive? related error!  Even with hash tables turned off.

/* [2008.11.13] Currently running this to test garbage collection under multithreaded conditions.

Running 10 tuples through is taking 12s with hoard and 17s with the
default allocator (using simple reference counting, 42 threads).  Of
course, this is abysmal because the single threaded version takes 1.6
seconds.

It presently segfaults sometimes with -gc ref, and segfaults often
with boehm (and sometimes gets a bus error).  The memory usage with
boehm seems to expand rapidly to >225mb.  And the collection times
likewise go up to >200ms.  (Extrapolating from 2 tuples to 60 would
leave it with a total collection time of 20s!  Or worse if the heap
kept growing...).

Ah, well duh, the accumulated FIFO nodes (from the over-running
timers) create extra heap for the GC to trace.  But I don't know if
they fully account for the rapid growth in memory footprint...  (in
just -n 2, we are left with 13 million extra in-flight fifo elements
after only a few seconds!  And that's with accelerator=20...).

Hmm... even with accelerator turned down to 1, it uses less than 100%
cpu (with all threads), but we still end up with 13,000 in-flight
tuples at the end.  It seems like I really need to switch to bounded
fifos.

*/


include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced_ints = detector((ch1i,ch2i,ch3i,ch4i));

include "marmot2.ws";

nodes = [(100, -0.0, 0.000891, 222.746048)];

include "marmot_heatmap.ws";

doas = oneSourceAMLTD(synced_ints, 4096);

noderecs = stream_map(fun(doavec) (nodes`List:ref(0), doavec), doas)

axes = (-2000.0, 15801.0, -11659.0, 6142.0)
grid_scale = 25.0
//grid_scale = 50.0
//grid_scale = 100.0
//grid_scale = 138.0
//grid_scale = 2000.0

clusters = temporal_cluster_amls(1,noderecs)

heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);


BASE <- common_backend(heatmaps, axes, grid_scale)

/*
BASE <- iterate heatmap in heatmaps {
  print("Got heatmap.\n");

  pic = colorize_likelihoods(heatmap);
  
  let (mx,u,v) = getmax(heatmap);
  let (x,y) = convertcoord(axes,grid_scale, u,v);
  println("Max marmot likelihood was "++mx++
          " at position "++x++","++y++
	  " w/pic coord "++u++","++v);
  draw_marmot(pic, u, v, f2i$ max(4.0, 250.0 / grid_scale));

  file = "temp.ppm";
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}
*/

