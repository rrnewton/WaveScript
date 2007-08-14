
// [2007.08.14] RRN: This is compiling in 1.5min instead of 5 min with
// interpret-meta instead of static-elaborate.  It uses 37.5 user
// time... In opt-lvl 3 it took 1 min / 34sec...

include "nodelocs.ws";
include "ensbox_logger.ws";
include "stdlib.ws";
include "netsource.ws";
include "timersource.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

axes :: (Float * Float * Float * Float);
axes = (-6303.0, 2122.0, -1410.0, 15517.0);

desired_min_pixel_dimm = 300
grid_scale = { 
  let (xmin, xmax, ymin, ymax) = axes;
  min(absF(xmax - xmin) / desired_min_pixel_dimm`i2f,
      absF(ymax - ymin) / desired_min_pixel_dimm`i2f)
}

fun toFloat(synced_ints)
  stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

nodes = [node1, node2, node3, node4, node5, node6, node7, node8]

ips = ["192.168.11.100",
       "192.168.11.103",
       "192.168.11.104",
       "192.168.11.108",
       "192.168.11.109",
       "192.168.11.112",
       "192.168.11.113",
       "192.168.11.115"]

synced = map(fun(ip) netsub_4sigseg(ip,"detections"), ips)

floats = map(toFloat,synced);

include "marmot2.ws";
include "marmot_heatmap.ws";

fun aml(slsf) {
  oneSourceAMLTD(snoop("DETECTION SEGMENTS",slsf), micgeometry, 4096);
}

fun lifted_norm((x,st)) (normalize_doas(x),st)

amls = 
  map(fun(floatdata)
      stream_map(fun((x,st)) (node1,st,x), stream_map(lifted_norm, aml(floatdata))),
      floats)

merged :: Stream TaggedAML;
merged = List:fold1(merge, amls)

clusters :: Stream (List TaggedAML);
clusters = temporal_cluster_amls(3, merged);

heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);

final = iterate (heatmap,stamp) in heatmaps {
  state { cnt = 0 }
  
  pic = colorize_likelihoods(heatmap);

  let (mx,u,v) = getmax(heatmap);
  let (x,y) = convertcoord(axes,grid_scale,u,v);
  println("Max marmot likelihood was "++mx++
          " at position "++x++","++y++
	  " w/pic coord "++u++","++v);
  draw_marmot(pic, u, v, f2i$ max(4.0, 250.0 / grid_scale));

  file = "pic"++1000+cnt++"_"++stamp++".ppm";
  cnt += 1;
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}

// COMMENT OUT WHEN USING THE PTOLEMY ENTRY POINT:
BASE <- final
