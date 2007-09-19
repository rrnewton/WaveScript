

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
