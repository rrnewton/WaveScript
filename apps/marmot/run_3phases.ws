

include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced_ints = detector((ch1i,ch2i,ch3i,ch4i));

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";
include "marmot_heatmap.ws";

doas = oneSourceAMLTD(synced, 4096);

nodes = [(100, -0.0, 0.000891, 222.746048)];

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


