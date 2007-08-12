

include "stdlib.ws";

// ================================================================================

// NOTE: THESE OLD FILES ARE 24KHZ!!

// These are the nodes we have data for.
//nodenums = [100, 103, 104, 108, 109, 112, 113]
//nodenums = [100, 103]

nodes = 
  [(100, -0.0, 0.000891, 222.746048),
   (103, 13282.529583, -4587.759179, 17.01309)]
/*
  [(100, -0.0, 0.000891, 222.746048),
   (103, 13282.529583, -4587.759179, 17.01309),
   (104, 7663.663345, -6068.768937, 244.168915),
   (108, 0.0, -4890.001043, 181.457764),
   (109, 13801.727366, -924.578002, 129.536758),
   (112, 6719.793799, 552.692044, 131.25087)
   (113, 679.793799, 5852.692044, 131.25087)]
*/


samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg
winsize = 4 * 4096;
//winsize = 16384;
//winsize = 16;

fun read_audio((id, _,_,_)) {
  fn = "multinode48khz/"++id++".raw";
  
  driver = timer(samp_rate * 4.0 / winsize`i2f);
  chans = (readFile(fn, "mode: binary window: "++winsize, driver) :: Stream Sigseg (Int16));

  fun onechan(offset)
    iterate w in chans {
      size = w`width / 4;
      assert_eq("source stream multiple of 4", w`width, size * 4);
      arr = Array:build(size, fun (i) (w[[(i*4) + offset]]));
      emit toSigseg(arr, w`start / 4`intToInt64 , w`timebase)
  };
  
  (onechan(0), onechan(1), onechan(2), onechan(3))  
  //[onechan(0), onechan(1), onechan(2), onechan(3)]
}

// When we're not live we just print log messages to the stream.
fun log(l,s) println(s)

//alldata :: List (List (Stream (Sigseg t)));
alldata = map(read_audio, nodes)

//BASE <- unionList(List:fold1(List:append, alldata))

// ================================================================================

include "marmot_first_phase.ws";

/*

let (_ch1,ch2,ch3,ch4) = List:ref(alldata, 0);

ch1 = snoop_every(100, fun (ind,x) ("Reading data window #: "++ind + 1`gint), _ch1);

BASE <- detector((ch1,ch2,ch3,ch4))

*/

alldetections = map(detector, alldata)

LSLS_map :: ((a -> b), List (Stream (List (Sigseg a)))) ->
                       List (Stream (List (Sigseg b)));
fun LSLS_map(fn, lsls)
  map(fun (sls)
       stream_map(fun (ls)
	   map(fun(seg) sigseg_map(fn,seg), 
           ls),
       sls),
  lsls);

allfloats = LSLS_map(int16ToFloat, alldetections)

/*

allfloats =
  map(fun (sls)
       stream_map(fun (ls)
	   map(fun(seg) sigseg_map(int16ToFloat,seg), 
           ls),
       sls),
  alldetections);

allfloats =
  map(fun ((ss1, ss2, ss3, ss4))
      (deep_stream_map(int16ToFloat, ss1),
       deep_stream_map(int16ToFloat, ss2),
       deep_stream_map(int16ToFloat, ss3),
       deep_stream_map(int16ToFloat, ss4)),
      alldata)


allfloats = {
  using Curry;
  (map$ map$ deep_stream_map$ int16ToFloat)(alldata)
}

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);
*/

// ================================================================================

include "marmot2.ws";


//(Stream (List (Sigseg Float)), Matrix Float, Int) -> Stream (Array Float);

fun aml(slsf) oneSourceAMLTD(slsf, micgeometry, 4096)

allamls = map(aml, allfloats)

include "marmot_heatmap.ws";

axes = (-2000.0, 15801.0, -11659.0, 6142.0)
//grid_scale = 25.0
//grid_scale = 50.0
//grid_scale = 100.0
//grid_scale = 138.0
grid_scale = 2000.0


//noderecs = List:map2(fun(node, doas) (node,doas), doas)

using List;

labeledamls = 
  List:mapi(fun(ind,doastream) 
      stream_map(fun(doas) (nodes.ref(ind), doas), doastream),
      allamls)

merged = List:fold1(merge, labeledamls)

clusters = stream_map(
  fun (clust) { 
    print("Got a cluster of detections from nodes: {");
    List:foreach(fun (((id,_,_,_),_)) print(id++" "), clust);
    print("}\n");
    clust;
  },
  temporal_cluster_amls(merged))

coordsys = coord_converters(axes, grid_scale);

heatmaps = stream_map(fun(x) doa_fuse(coordsys,x), clusters);

BASE <- heatmaps

//BASE <- allamls.ref(0)
//BASE <- gnuplot_array_stream(allamls.ref(0))

//BASE <- allfloats.ref(0)
//BASE <- allfloats.ref(0).ref(0)

/*



BASE <- iterate heatmap in heatmaps {
  let (_,_,cx,cy) = coordsys;

  pic = colorize_likelihoods(heatmap);

  let (mx,i,j) = getmax(heatmap,coordsys);
  println("Max marmot likelihood was "++mx++
          " at position "++cx(j)++","++cy(i)++
	  " w/pic coord "++i++","++j);
  draw_marmot(pic, i, j, f2i$ max(4.0, 250.0 / grid_scale));

  file = "temp.ppm";
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}


*/
