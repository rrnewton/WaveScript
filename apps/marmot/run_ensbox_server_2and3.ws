
// [2007.08.14] RRN: This is compiling in 1.5min instead of 5 min with
// interpret-meta instead of static-elaborate.  It uses 37.5 user
// time... In opt-lvl 3 it took 1 min / 34sec...

include "nodelocs.ws";
include "types.ws";

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

nodes = [node1, node2, node3, node4, node5, node6, node7, node8]

ips = ["192.168.11.100",
       "192.168.11.103",
       "192.168.11.104",
       "192.168.11.108",
       "192.168.11.109",
       "192.168.11.112",
       "192.168.11.113",
       "192.168.11.115"]


tag :: List (Stream t) -> List (Stream (NodeRecord * t));
fun tag(sls) 
  map(fun((node,strms))
      stream_map(fun(x) (node,x) ,strms),
      List:zip(nodes,sls))

include "marmot2.ws";
include "marmot_heatmap.ws";

// **********************  UNCOMMENT FOR SERVER SIDE AML ************************ //
//================================================================================//

synced = map(fun(ip) snoop("DETECTION SEGMENTS", netsub_4sigseg(ip,"detections")), ips)
amls :: List (Stream AML);
amls = map(fun (slsf) oneSourceAMLTD(slsf, 4096),synced)


// **********************  UNCOMMENT FOR CLIENT SIDE AML ************************ //
//================================================================================//

//synced = map(fun(ip) netsub_amls(ip,"amls"), ips)
//amls = map(fun (slsf) oneSourceAMLTD(slsf, 4096),synced)


//================================================================================//



merged :: Stream (Tagged AML);
merged = List:fold1(merge, tag(amls))

clusters :: Stream (List (Tagged AML));
clusters = temporal_cluster_amls(3, merged);

heatmaps :: Stream LikelihoodMap;
heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);

final = iterate lhoodmap in heatmaps {
  state { cnt = 0 }

  pic = colorize_likelihoods(lhoodmap);

  let (mx,u,v) = getmax(lhoodmap);
  let (x,y) = convertcoord(axes,grid_scale,u,v);

  println("Max marmot likelihood was "++mx++
          " at position "++x++","++y++
	  " w/pic coord "++u++","++v);
  draw_marmot(pic, u, v, f2i$ max(4.0, 250.0 / grid_scale));

  let (_,stamp) = lhoodmap;
  file = "pic"++1000+cnt++"_"++stamp++".ppm";
  cnt += 1;
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}

// COMMENT OUT WHEN USING THE PTOLEMY ENTRY POINT:
BASE <- final
