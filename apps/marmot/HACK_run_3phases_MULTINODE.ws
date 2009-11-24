

include "stdlib.ws";

LOG_TIMING = 255;

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

node1 = (100, -0.700000,    14107.000732, 13.200000)
node2 = (103, -4106.700134, 12947.200012, 20.299999)
node3 = (104, 1420.499992,  10212.300110, -10.700000)
node4 = (108, -3468.399811, 9284.500122,  4.500000)
node5 = (109, 773.099995,   6791.600037,  -161.100006)
node6 = (112, -5601.399994, 4740.200043,  -9.200000)
node7 = (113, 0.000000,     0.000000,     -10.100000)

axes :: (Float * Float * Float * Float);
axes = (-6303.0, 2122.0, -1410.0, 15517.0);

// AUTO GRID SIZE:
desired_min_pixel_dimm = 300
grid_scale = { 
  let (xmin, xmax, ymin, ymax) = axes;
  min(absF(xmax - xmin) / desired_min_pixel_dimm`i2f,
      absF(ymax - ymin) / desired_min_pixel_dimm`i2f)
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

SLS_map :: ((a -> b), Stream (List (Sigseg a))) ->
                     (Stream (List (Sigseg b))) ;
fun SLS_map(fn, sls)
    stream_map(fun (ls)
         map(fun(seg) sigseg_map(fn,seg), 
	     ls),
       sls)

// ================================================================================
include "marmot_first_phase.ws";

det1 = SLS_map(int16ToFloat, detector(read_audio(node1)))
det2 = SLS_map(int16ToFloat, detector(read_audio(node2)))
det3 = SLS_map(int16ToFloat, detector(read_audio(node3)))
det4 = SLS_map(int16ToFloat, detector(read_audio(node4)))
det5 = SLS_map(int16ToFloat, detector(read_audio(node5)))
det6 = SLS_map(int16ToFloat, detector(read_audio(node6)))
det7 = SLS_map(int16ToFloat, detector(read_audio(node7)))

// ================================================================================
include "marmot2.ws";

fun aml(slsf) 
  oneSourceAMLTD(slsf, micgeometry, 4096);

// ================================================================================
include "marmot_heatmap.ws";

aml1 = stream_map(fun(x) (node1,x), stream_map(normalize_aml,aml(det1)))
aml2 = stream_map(fun(x) (node2,x), stream_map(normalize_aml,aml(det2)))
aml3 = stream_map(fun(x) (node3,x), stream_map(normalize_aml,aml(det3)))
aml4 = stream_map(fun(x) (node4,x), stream_map(normalize_aml,aml(det4)))
aml5 = stream_map(fun(x) (node5,x), stream_map(normalize_aml,aml(det5)))
aml6 = stream_map(fun(x) (node6,x), stream_map(normalize_aml,aml(det6)))
aml7 = stream_map(fun(x) (node7,x), stream_map(normalize_aml,aml(det7)))

merged :: Stream TaggedAML;
merged = 
  merge(aml1, 
  merge(aml2, 
  merge(aml3, 
  merge(aml4, 
  merge(aml5, 
  merge(aml6, 
        aml7))))))

clusters = temporal_cluster_amls(3, merged);

heatmaps = stream_map(fun(x) doa_fuse(axes,grid_scale,x), clusters);

BASE <- iterate heatmap in heatmaps {
  state { cnt = 0 }
  
  pic = colorize_likelihoods(heatmap);

  let (mx,u,v) = getmax(heatmap);
  let (x,y) = convertcoord(axes,grid_scale,u,v);
  println("Max marmot likelihood was "++mx++
          " at position "++x++","++y++
	  " w/pic coord "++u++","++v);
  draw_marmot(pic, u, v, f2i$ max(4.0, 250.0 / grid_scale));

  file = "temp"++cnt++".ppm";
  cnt += 1;
  write_ppm_file(file,pic);

  emit ("Wrote image to file: " ++ file);
}
