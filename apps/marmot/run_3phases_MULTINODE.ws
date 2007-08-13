

include "stdlib.ws";

// ================================================================================

// NOTE: THESE OLD FILES ARE 24KHZ!!

// These are the nodes we have data for.
//nodenums = [100, 103, 104, 108, 109, 112, 113]
//nodenums = [100, 103]

//axes = (-6500.0, 2500.0, -100.0, 14000.0)


//axes = (-2000.0, 15801.0, -11659.0, 6142.0)
//axes = (-4000.0, 31602.0, -23318.0, 12284.0)
//axes = (-4500.0, 30801.0, -22659.0, 12142.0)
//axes = (-20000.0, 158010.0, -116590.0, 61420.0)
//axes = (-40000.0, 308010.0, -226590.0, 120420.0)

//axes = (-4500.0, 1001.0, 0.0, 15142.0)

nodes = 
  [
   (100, -0.700000, 14107.000732, 13.200000),
   (103, -4106.700134, 12947.200012, 20.299999),
   (104, 1420.499992, 10212.300110, -10.700000),
   (108, -3468.399811, 9284.500122, 4.500000),
   (109, 773.099995, 6791.600037, -161.100006),
   (113, 0.000000, 0.000000, -10.100000),
   (112, -5601.399994, 4740.200043, -9.200000)

    //(115, -4438.600159, -516.499996, 10.800000),
   ]

/*
  [(100, -0.0, 0.000891, 222.746048),
   (103, 13282.529583, -4587.759179, 17.01309),
   (104, 7663.663345, -6068.768937, 244.168915),
   (108, 0.0, -4890.001043, 181.457764),
   (109, 13801.727366, -924.578002, 129.536758),
   (112, 6719.793799, 552.692044, 131.25087)
   (113, 679.793799, 5852.692044, 131.25087)]
*/

axes :: (Float * Float * Float * Float);
axes = {
  using List;
  let (_,x0,y0,_) = nodes.ref(0);
  /*
  let (xmin,xmax,ymin,ymax) =
    List:fold(fun((xmn,xmx,ymn,ymx), (id,x,y,yaw))
	      (min(x,xmn), max(x,xmx), min(y,ymn), max(y,ymx)),
//	      (x0,x0,y0,y0),
	      (0.0,10.0,0.0,10.0),
	      nodes);
  */
  xmin = fold(min, x0, map(fun((_,x,_,_)) x, nodes));
  xmax = fold(max, x0, map(fun((_,x,_,_)) x, nodes));
  ymin = fold(min, y0, map(fun((_,_,y,_)) y, nodes));
  ymax = fold(max, y0, map(fun((_,_,y,_)) y, nodes));

  padx = (xmax - xmin) * 0.1;
  pady = (ymax - ymin) * 0.1;
  (xmin-padx, xmax+padx, ymin-pady, ymax+pady)
  //(xmin,xmax, ymin,ymax)
}

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

*/

// Synced data from each node
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

// Synced data from each node (float format)
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

fun aml(slsf) 
  oneSourceAMLTD(slsf, micgeometry, 4096)

// AML results from each node.
allamls :: List (Stream (Array Float));
allamls = map(aml, allfloats)

include "marmot_heatmap.ws";

//grid_scale = 25.0
//grid_scale = 50.0
//grid_scale = 100.0
//grid_scale = 138.0
//grid_scale = 2000.0

// AUTO GRID SIZE:
desired_min_pixel_dimm = 300
grid_scale = { 
  let (xmin, xmax, ymin, ymax) = axes;
  min(absF(xmax - xmin) / desired_min_pixel_dimm`i2f,
      absF(ymax - ymin) / desired_min_pixel_dimm`i2f)
}

//BASE <- iterate _ in timer(3.0) { println("gridscale "++grid_scale); emit axes }

labeledamls = 
  List:map(fun((nd,amlstrm)) 
             stream_map(fun(aml) (nd,aml), amlstrm), 
           List:zip(nodes, allamls))


merged :: Stream TaggedAML;
merged = List:fold1(merge, labeledamls)

//BASE <- merged

clusters = stream_map(
  fun (clust) { 
    print("Got a cluster of detections from nodes: {");
    List:foreach(fun (((id,_,_,_),_)) print(id++" "), clust);
    print("}\n");
    clust;
  },
  temporal_cluster_amls(merged))

//BASE <- clusters

coordsys = coord_converters(axes, grid_scale);

heatmaps = stream_map(fun(x) doa_fuse(coordsys,x), clusters);

//BASE <- heatmaps


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

//BASE <- heatmaps
//using List;
//BASE <- allamls.ref(0)
//BASE <- gnuplot_array_stream(allamls.ref(0))

//BASE <- allfloats.ref(0)
//BASE <- allfloats.ref(0).ref(0)

/*


*/








//noderecs = List:map2(fun(node, doas) (node,doas), doas)

/*
labeledamls = 
  List:map(fun(()) 
      stream_map(fun(doas) (nodes.ref(ind), doas), doastream),
      allamls)
*/
// Yuck, it's a bit of a hassle to reassociate the noderecords.


