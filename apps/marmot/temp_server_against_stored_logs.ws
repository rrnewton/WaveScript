
// [2007.08.14] RRN: This is compiling in 1.5min instead of 5 min with
// interpret-meta instead of static-elaborate.  It uses 37.5 user
// time... In opt-lvl 3 it took 1 min / 34sec...

include "nodelocs.ws";
include "types.ws";

include "ensbox_logger.ws";
include "stdlib.ws";
include "gnuplot.ws";
include "netsource.ws";
include "timersource.ws";
include "ptolemy.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

nodes = [node1, node2, node3, node4, node5, node6, node7, node8]

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


logdata = HACK_O_RAMA("/marmot_datasets/2007_08_16/1_dataset_with_a_spurious_prefix/detections.scheme.log")

//synced :: Stream (List (Sigseg Int16));
synced :: List (Stream (Sigseg Int16));
synced = [smap(fun((a,b,c,d)) a, logdata),
	  smap(fun((a,b,c,d)) b, logdata),
	  smap(fun((a,b,c,d)) c, logdata),
	  smap(fun((a,b,c,d)) d, logdata)]

_amls :: List (Stream AML);
_amls = map(fun (slsf) oneSourceAMLTD(slsf, 4096), synced)

amls :: List (Stream AML);
amls = map(fun(((id,_,_,yaw),amlstrm)) 
                maybe_graph_aml(id, yaw, amlstrm),
           List:zip(nodes, _amls))

merged :: Stream (Tagged AML);
merged = List:fold1(merge, tag(amls))

clusters :: Stream (List (Tagged AML));
clusters = temporal_cluster_amls(3, merged);

heatmaps :: Stream LikelihoodMap;
heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);

final = dump_likelihood_maps(heatmaps, axes, grid_scale)

BASE <- final
