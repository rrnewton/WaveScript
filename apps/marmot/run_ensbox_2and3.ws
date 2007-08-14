
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

synced1 =  netsub_4sigseg("192.168.11.100", "detections"); 
synced2 =  netsub_4sigseg("192.168.11.103", "detections"); 
synced3 =  netsub_4sigseg("192.168.11.104", "detections"); 
synced4 =  netsub_4sigseg("192.168.11.108", "detections"); 
synced5 =  netsub_4sigseg("192.168.11.109", "detections"); 
synced6 =  netsub_4sigseg("192.168.11.112", "detections"); 
synced7 =  netsub_4sigseg("192.168.11.113", "detections"); 
synced8 =  netsub_4sigseg("192.168.11.115", "detections"); 

floats1 = toFloat$ synced1
floats2 = toFloat$ synced2
floats3 = toFloat$ synced3
floats4 = toFloat$ synced4
floats5 = toFloat$ synced5
floats6 = toFloat$ synced6
floats7 = toFloat$ synced7
floats8 = toFloat$ synced8

include "marmot2.ws";
include "marmot_heatmap.ws";

fun aml(slsf) {
  oneSourceAMLTD(snoop("DETECTION SEGMENTS",slsf), micgeometry, 4096);
}

fun lifted_norm((x,st)) (normalize_doas(x),st)

aml1 = stream_map(fun((x,st)) (node1,st,x), stream_map(lifted_norm, aml(floats1 )))
aml2 = stream_map(fun((x,st)) (node2,st,x), stream_map(lifted_norm, aml(floats2 )))
aml3 = stream_map(fun((x,st)) (node3,st,x), stream_map(lifted_norm, aml(floats3 )))
aml4 = stream_map(fun((x,st)) (node4,st,x), stream_map(lifted_norm, aml(floats4 )))
aml5 = stream_map(fun((x,st)) (node5,st,x), stream_map(lifted_norm, aml(floats5 )))
aml6 = stream_map(fun((x,st)) (node6,st,x), stream_map(lifted_norm, aml(floats6 )))
aml7 = stream_map(fun((x,st)) (node7,st,x), stream_map(lifted_norm, aml(floats7 )))
aml8 = stream_map(fun((x,st)) (node8,st,x), stream_map(lifted_norm, aml(floats8 )))

merged :: Stream TaggedAML;
merged = 
  merge(aml1, 
  merge(aml2, 
  merge(aml3, 
  merge(aml4, 
  merge(aml5, 
  merge(aml6, 
  merge(aml7, 
        aml8)))))))

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
//BASE <- final
