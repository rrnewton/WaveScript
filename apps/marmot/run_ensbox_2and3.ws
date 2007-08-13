


node1 = (100, -0.700000,    14107.000732, 13.200000)
node2 = (103, -4106.700134, 12947.200012, 20.299999)
node3 = (104, 1420.499992,  10212.300110, -10.700000)
node4 = (108, -3468.399811, 9284.500122,  4.500000)
node5 = (109, 773.099995,   6791.600037,  -161.100006)
node6 = (112, -5601.399994, 4740.200043,  -9.200000)
node7 = (113, 0.000000,     0.000000,     -10.100000)
node8 = (115, -4438.600159, -516.499996, 10.800000)


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

synced1 = toFloat$ netsub_4sigseg("192.168.11.100", "detections"); 
synced2 = toFloat$ netsub_4sigseg("192.168.11.103", "detections"); 
synced3 = toFloat$ netsub_4sigseg("192.168.11.104", "detections"); 
synced4 = toFloat$ netsub_4sigseg("192.168.11.108", "detections"); 
synced5 = toFloat$ netsub_4sigseg("192.168.11.109", "detections"); 
synced6 = toFloat$ netsub_4sigseg("192.168.11.112", "detections"); 
synced7 = toFloat$ netsub_4sigseg("192.168.11.113", "detections"); 
synced8 = toFloat$ netsub_4sigseg("192.168.11.115", "detections"); 

include "marmot2.ws";
include "marmot_heatmap.ws";

fun aml(slsf) {
  oneSourceAMLTD(snoop("DETECTION SEGMENTS",slsf), micgeometry, 4096);
}

fun lifted_norm((x,st)) (normalize_doas(x),st)

aml1 = stream_map(fun((x,st)) (node1,st,x), stream_map(lifted_norm, aml(synced1 )))
aml2 = stream_map(fun((x,st)) (node2,st,x), stream_map(lifted_norm, aml(synced2 )))
aml3 = stream_map(fun((x,st)) (node3,st,x), stream_map(lifted_norm, aml(synced3 )))
aml4 = stream_map(fun((x,st)) (node4,st,x), stream_map(lifted_norm, aml(synced4 )))
aml5 = stream_map(fun((x,st)) (node5,st,x), stream_map(lifted_norm, aml(synced5 )))
aml6 = stream_map(fun((x,st)) (node6,st,x), stream_map(lifted_norm, aml(synced6 )))
aml7 = stream_map(fun((x,st)) (node7,st,x), stream_map(lifted_norm, aml(synced7 )))
aml8 = stream_map(fun((x,st)) (node8,st,x), stream_map(lifted_norm, aml(synced8 )))

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

BASE <- final
