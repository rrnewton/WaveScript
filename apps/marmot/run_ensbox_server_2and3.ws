
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

nodes = [node1, node2, node3, node4, node5, node6, node7] //, node8]

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

fun ip(id) "192.168.11."++id

amls :: List (Stream AML);

// **********************  SERVER SIDE AML ************************ //
//==================================================================//

/* because show sigseg doesnt dump it */
fun snoop_4sigseg_to_file(id,f,s) {
  iterate x in s {    
    write_to_file(f,
	 "Detection segments: " ++ id ++ " " ++ show(x) ++ "\n" ++
	 show(List:map(toArray,x)) ++ 
	 "\n");
    emit(x) 
  }
}

synced = map(fun((id,_,_,_)) 
      snoop_4sigseg_to_file(id,"/home/girod/marmots/detections.log",
      snoop("DETECTION SEGMENTS "++id, netsub_4sigseg(ip(id),"detections"))), 
      nodes)


amls_server = map(fun (slsf) 
      smap(normalize_aml,
        snoop_to_file("/home/girod/marmots/amls.log",
                    oneSourceAMLTD(slsf, 4096))),
      synced)

// **********************  CLIENTSIDE AML ************************ //
//=================================================================//

// We get int16s, we convert to floats, and then normalize.
amls_client = map(fun((id,_,_,_))
      snoop_to_file("/home/girod/marmots/client_amls.log",
                    smap(fun(aml) normalize_aml(aml_to_floats(aml)), 
		         netsub_amls(ip(id),"amls"))), nodes)

// **********************  ADAPTIVE AML ************************** //
//=================================================================//

_amls = if AMLSERVERSIDE 
	then amls_server 
	else if AMLADAPTIVE 
	  then map(fun ((x,y)) (merge(x,y)), List:zip(amls_server,amls_client))
	  else amls_client

//================================================================================//

amls :: List (Stream AML);
amls = map(fun(((id,_,_,yaw),amlstrm)) 
	   amlstrm, //                maybe_graph_aml(id, yaw, amlstrm),
           List:zip(nodes, _amls))

//amls = map(fun(((id,_,_,yaw),strm)) aml_detections(id, yaw, strm),
//              List:zip(nodes,alldetections))

merged :: Stream (Tagged AML);
merged = List:fold1(merge, tag(amls))

clusters :: Stream (List (Tagged AML));
clusters = temporal_cluster_amls(3, merged);

heatmaps :: Stream LikelihoodMap;
heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);

ignored = draw_multi_amls(nodes,amls)
ignored2 = draw_multi_detections(nodes,synced)

final = 
merge(ignored, //merge(ignored,ignored2),
      dump_likelihood_maps(heatmaps, axes, grid_scale))

// COMMENT OUT WHEN USING THE PTOLEMY ENTRY POINT:
BASE <- final
