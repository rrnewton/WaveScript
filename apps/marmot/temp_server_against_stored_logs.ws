
// [2007.08.14] RRN: This is compiling in 1.5min instead of 5 min with
// interpret-meta instead of static-elaborate.  It uses 37.5 user
// time... In opt-lvl 3 it took 1 min / 34sec...

include "nodelocs.ws";
include "types.ws";

//include "ensbox_logger.ws";
fun log(l,s)      print(s++"\n")
fun log_file(l,s) print(s++"\n")
fun timer_source(_,t) timer(1000.0 / t`intToFloat)

include "stdlib.ws";
include "gnuplot.ws";
//include "netsource.ws";
//include "timersource.ws";
//include "ptolemy.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

//nodes = [node1, node2, node3, node4, node5, node6, node7, node8]
nodes = nodels;

include "marmot2.ws";
include "marmot_heatmap.ws";

axes :: (Float * Float * Float * Float);
axes = auto_axes(nodes)

desired_min_pixel_dimm = 300
grid_scale = { 
  let (xmin, xmax, ymin, ymax) = axes;
  min(absF(xmax - xmin) / desired_min_pixel_dimm`i2f,
      absF(ymax - ymin) / desired_min_pixel_dimm`i2f)
}

tag :: List (Stream t) -> List (Stream (NodeRecord * t));
fun tag(sls) 
  map(fun((node,strms))
      stream_map(fun(x) (node,x) ,strms),
      List:zip(nodes,sls))

//logdata = HACK_O_RAMA("/marmot_datasets/2007_08_16/1_dataset_with_a_spurious_prefix/detections.scheme.log")
logdata = 
iterate x in HACK_O_RAMA("/marmot_datasets/2007_08_17_marmots6/detections.scheme.log") {
  print("Read from logfile...\n");
  emit x;
}

//synced :: Stream (List (Sigseg Int16));
synced :: List (Stream Detection);
//synced = smap(fun((a,b,c,d)) [a,b,c,d], logdata);
/*
synced = [smap(fun((a,b,c,d)) a, logdata),
	  smap(fun((a,b,c,d)) b, logdata),
	  smap(fun((a,b,c,d)) c, logdata),
	  smap(fun((a,b,c,d)) d, logdata)]
*/
synced = map(fun ((ndid,_,_,_)) smap(snd, sfilter(fun((id,det)) id==ndid, logdata)),
             nodes)

_amls :: List (Stream AML);
_amls = map(fun (slsf) oneSourceAMLTD(slsf, 4096), synced)

amls :: List (Stream AML);
amls = map(fun(((id,_,_,yaw),amlstrm)) 
	   //maybe_graph_aml(id, yaw, amlstrm),
	   amlstrm,
           List:zip(nodes, _amls))

merged :: Stream (Tagged AML);
merged = List:fold1(merge, tag(amls))

clusters :: Stream (List (Tagged AML));
clusters = temporal_cluster_amls(3, merged);

heatmaps :: Stream LikelihoodMap;
heatmaps = stream_map(fun(xx) doa_fuse(axes,grid_scale,xx), clusters);

//final = dump_txt_images(draw_likelihood_map(heatmaps, axes, grid_scale));
final = gen_ppm(draw_likelihood_map(heatmaps, axes, grid_scale));

ignored = draw_multi_amls(nodes,amls)

BASE <- final
/* BASE <- merge(ignored, clusters) */

/* BASE <- _amls`List:ref(0) */
//BASE <- synced`List:ref(0);
//BASE <- synced
