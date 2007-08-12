

include "stdlib.ws";

// ================================================================================

// NOTE: THESE OLD FILES ARE 24KHZ!!

// These are the nodes we have data for.
//nodenums = [100, 103, 104, 108, 109, 112, 113]
nodenums = [100, 103]
samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg
winsize = 4 * 4096;
//winsize = 16384;
//winsize = 16;

fun read_audio(id) {
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
  
  //  (onechan(0), onechan(1), onechan(2), onechan(3))  
  (onechan(0), onechan(1), onechan(2), onechan(3))
}

// When we're not live we just print log messages to the stream.
fun log(l,s) println(s)

alldata = map(read_audio, nodenums)

//BASE <- unionList(List:fold1(List:append, alldata))

// ================================================================================

include "marmot_first_phase.ws";

let (_ch1,ch2,ch3,ch4) = List:ref(alldata, 0);

ch1 = snoop_every(100, fun (ind,x) ("Reading data window #: "++ind + 1`gint), _ch1);

BASE <- detector((ch1,ch2,ch3,ch4))

/*

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);



// ================================================================================



include "marmot2.ws";
include "marmot_heatmap.ws";

doas = oneSourceAMLTD(synced, micgeometry, 4096);

nodes = [(100, -0.0, 0.000891, 222.746048)];

noderecs = stream_map(fun(doavec) (nodes`List:ref(0), doavec), doas)

axes = (-2000.0, 15801.0, -11659.0, 6142.0)
grid_scale = 25.0
//grid_scale = 50.0
//grid_scale = 100.0
//grid_scale = 138.0
//grid_scale = 2000.0

clusters = temporal_cluster_amls(noderecs)

coordsys = coord_converters(axes, grid_scale);

//BASE <- CONST(coordsys)

heatmaps = stream_map(fun(x) doa_fuse(coordsys,x), clusters);

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
