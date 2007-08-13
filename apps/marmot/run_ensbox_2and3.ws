




node1 = (100, -0.700000,    14107.000732, 13.200000)
node2 = (103, -4106.700134, 12947.200012, 20.299999)
node3 = (104, 1420.499992,  10212.300110, -10.700000)
node4 = (108, -3468.399811, 9284.500122,  4.500000)
node5 = (109, 773.099995,   6791.600037,  -161.100006)
node6 = (112, -5601.399994, 4740.200043,  -9.200000)
node7 = (113, 0.000000,     0.000000,     -10.100000)



include "ensbox_logger.ws";
include "stdlib.ws";
include "netsource.ws";

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

synced1 = toFloat$ netsub_4sigseg("192.168.11.100", "detections100"); 
synced2 = toFloat$ netsub_4sigseg("192.168.11.103", "detections103"); 
synced3 = toFloat$ netsub_4sigseg("192.168.11.104", "detections104"); 
synced4 = toFloat$ netsub_4sigseg("192.168.11.108", "detections108"); 
synced5 = toFloat$ netsub_4sigseg("192.168.11.109", "detections109"); 
synced6 = toFloat$ netsub_4sigseg("192.168.11.112", "detections112"); 
synced7 = toFloat$ netsub_4sigseg("192.168.11.113", "detections113"); 
synced8 = toFloat$ netsub_4sigseg("192.168.11.115", "detections115"); 

include "marmot2.ws";
include "marmot_heatmap.ws";

fun aml(slsf) oneSourceAMLTD(slsf, micgeometry, 4096);

aml1 = stream_map(fun(x) (node1,x), stream_map(normalize_doas,aml(synced1)))
aml2 = stream_map(fun(x) (node2,x), stream_map(normalize_doas,aml(synced2)))
aml3 = stream_map(fun(x) (node3,x), stream_map(normalize_doas,aml(synced3)))
aml4 = stream_map(fun(x) (node4,x), stream_map(normalize_doas,aml(synced4)))
aml5 = stream_map(fun(x) (node5,x), stream_map(normalize_doas,aml(synced5)))
aml6 = stream_map(fun(x) (node6,x), stream_map(normalize_doas,aml(synced6)))
aml7 = stream_map(fun(x) (node7,x), stream_map(normalize_doas,aml(synced7)))


merged :: Stream TaggedAML;
merged = 
  merge(aml1, 
  merge(aml2, 
  merge(aml3, 
  merge(aml4, 
  merge(aml5, 
  merge(aml6, 
        aml7))))))

raw_clusters = temporal_cluster_amls(merged);

clusters = iterate clust in raw_clusters {
  print("Got a cluster of detections from nodes: { ");
  List:foreach(fun (((id,_,_,_),_)) print(id++" "), clust);
  print("}\n");
  if List:length(clust) >= 3 then emit clust;
}

coordsys = coord_converters(axes, grid_scale);

heatmaps = stream_map(fun(x) doa_fuse(coordsys,x), clusters);

BASE <- iterate heatmap in heatmaps {
  state { cnt = 0 }
  
  let (_,_,cx,cy) = coordsys;

  pic = colorize_likelihoods(heatmap);

  let (mx,i,j) = getmax(heatmap,coordsys);
  println("Max marmot likelihood was "++mx++
          " at position "++cx(j)++","++cy(i)++
	  " w/pic coord "++i++","++j);
  draw_marmot(pic, i, j, f2i$ max(4.0, 250.0 / grid_scale));

  file = "temp"++cnt++".ppm";
  cnt += 1;
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}
