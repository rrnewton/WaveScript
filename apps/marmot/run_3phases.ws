

include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";
include "marmot_heatmap.ws";

doas = oneSourceAMLTD(synced, micgeometry, 4096);

nodes = [(100, -0.0, 0.000891, 222.746048)];

noderecs = stream_map(fun(doavec) (nodes`List:ref(0), doavec), doas)

axes = (-2000.0, 15801.0, -11659.0, 6142.0)
grid_scale = 25.0
//grid_scale = 50.0
//grid_scale = 100.0
//grid_scale = 138.0
//grid_scale = 2000.0

clusters = temporal_cluster_amls(noderecs)

coordsys = coord_converters(axes, grid_scale);

//BASE <- CONST(coordsys)

heatmaps = stream_map(fun(x) doa_fuse(coordsys,x), clusters);

BASE <- iterate heatmap in heatmaps {

  println("  Writing to ppm file...");

  pic = colorize_likelihoods(heatmap);

  file = "temp.ppm";
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}


