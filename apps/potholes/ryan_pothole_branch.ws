
include "stdlib.ws";
include "filter.ws";
//include "matrix.ws";


// Note: rrn: This currently takes 3.7 seconds to compile for me with
// iu-match everywhere, and 4.0 seconds with rn-match in the type
// checker!  Arg!  Renabling the non multi-value "optimization"
// doesn't help that at all.  
//
// With ws.opt the numbers are 3.5 and 3.3 seconds,
// respectively.... so it looks like the optimizer makes rn-match pull
// ahead.

// [2007.04.09] rrn: Using rn-match in the type checker makes this
// take 3.7 seconds as opposed to 4.0 seconds to compile.  With ws.opt
// it 3.3 as opposed to 3.5 seconds.
//
// I think the code bloat isn't great, but it still does better on
// speed.  Actually.. with this evaluation the vast majority of time
// is spent in the elaborator (3 seconds).  The first typecheck
// improves from 125 ms to 75ms with rn-match, which is quite
// substantial (especially since a major piece of the type-checker
// isn't converted to rn-match yet!)
// 
// The next target for rn-match will definitely be the static-elaborator.


//======================================================================



DEBUG=true;
     
detect :: Stream (Float * Sigseg t) -> Stream (Bool * Int * Int * Int * Float * Float * Sigseg t);
fun detect(scorestrm) {
  // Constants:
  alpha = 0.90;
  hi_thresh = 15;                   // thresh above ewma
  startup_init = 75;
  refract_interval = 0;             // set higher to merge.. e.g. 2 
  samples_padding = 100;

  iterate((score,win) in scorestrm) {
    state {
      thresh_value = 0.0;
      trigger = false;
      smoothed_mean = 0.0;
      _start = 0; 
      trigger_value = 0.0;
      startup = startup_init;
      refract = 0;                 

      // private
      noise_lock = 0; // stats

      max_peak = 0.0;
      peak_location = 0;
      integral = 0.0;
    }

    st = win`start;
    en = win`end;   

    fun reset() {
      thresh_value := 0.0;
      trigger := false;
      smoothed_mean := 0.0;
      _start := 0;
      trigger_value := 0.0;
      startup := startup_init;
      refract := 0;
    };

    if DEBUG then {
      temp = (if trigger then 1 else 0);
      println("@# " ++show(thresh_value)++ " " ++show(temp)++ 
	  " " ++show(smoothed_mean)++ 
	  " " ++show(_start)++ " " ++show(trigger_value));
    };
    
    /* if we are triggering.. */
    if trigger then {      

      integral := integral + score;
      if (score > max_peak) then {
        max_peak := score;
        peak_location := st + en / 2;
      };

      /* over thresh.. set refractory */
      if score > thresh_value then {
	refract := refract_interval;
      } else if refract > 0 then {	
	/* refractory counting down */
	refract := refract - 1;
      }	else {
	/* untriggering! */
	trigger := false;

	emit (true, _start, en, peak_location, max_peak, integral, win); // end sample
	if DEBUG then
	print("KEEP message: "++show((true, _start - samples_padding, en + samples_padding))++
	      " just processed window "++show(st)++":"++show(en)++"\n");

	// ADD TIME! // Time(casted->_first.getTimebase()
	_start := 0;
        thresh_value := 0.0;
      }
    };

      /* compute thresh */
      let thresh = i2f(hi_thresh) + smoothed_mean;

      if DEBUG then 
        print("Thresh to beat: "++show(thresh)++ " " ++ smoothed_mean++ ", Current Score: "++show(score)++
 " "++refract++ " " ++ startup ++"\n");

      /* over thresh and not in startup period (noise est period) */
      if startup == 0 && refract == 0 && score > thresh then {
	if DEBUG then print("Switching trigger to ON state.\n");
	trigger := true;
	refract := refract_interval;
	thresh_value := thresh;
	_start := st;
	trigger_value := score;
	max_peak = 0;
	integral = 0;
      };

	/* otherwise, update the smoothing filters */
	smoothed_mean := score *. (1.0 -. alpha) +. smoothed_mean *. alpha;
      if (score < smoothed_mean) then smoothed_mean := score;

      /* count down the startup phase */
      if startup > 0 then startup := startup - 1;
      
      /* ok, we can free from sync */
      /* rrn: here we lamely clear from the beginning of time. */
      /* but this seems to assume that the sample numbers start at zero?? */
      emit (false, 0, st - 1, 0, 0.0, 0.0, win);
      if DEBUG then 
      print("DISCARD message: "++show((false, 0, max(0, en - samples_padding)))++
	    " just processed window "++show(st)++":"++show(en)++"\n");
      
  }
}



fun specgram_seglist(ext) {
  iterate (segs in ext) {
	
    state {
      xcount = 0;
    }
    
    skip = 8;
    points = 256;
    w = 2048;
    
    s1 = List:ref(segs,0);
    s2 = List:ref(segs,1);
    s3 = List:ref(segs,2);
    
    hw = gaussian(intToFloat(skip),points);
    
    for j = 0 to points-1 {
      println("## " ++ j ++ " " ++ hw[j]);
    };
    
    //specgram the sync'd data 
    
    fun dospec(index,i,x) {
      s = List:ref(segs,index);
      a = toArray(s.subseg(s.start+i,points));
      win = apairmult(a,hw);
      f = fftR2C(win);
      for j = 0 to points/2 {
	println("@@ " ++ x ++ " " ++ j+index*(points/2+1) ++ " " ++ absC(f[j])
		++ " " ++ s.start+i);
      };
    };
    
    for i = 0 to ((s1.width-points) / skip) {
      dospec(0,i*skip,xcount);
      dospec(1,i*skip,xcount);
      dospec(2,i*skip,xcount);
      println("@@ ");
      xcount := xcount + 1;
    };	
    
    for i = 0 to s1.width-1 {
      println("@# " ++ s1.start+1 ++ " "
	      ++ s1[[i]] ++ " " 
	      ++ s2[[i]] ++ " " 
	      ++ s3[[i]] ++ " " );
    };
    
  xcount := xcount + 10;
  };
};


//sm = fun(f) fun(s) stream_map(f,s);
sm = stream_map;

// ============================================================
// Main query:

// This query reads in:
//   (Timestamp, Lat, Long, X,Y,Z)
// Where X/Y/Z is accelerometer data.
//
chans as (time,lat,long,x,y,z) = 
 (readFile("/tmp/clip", "")
 //(readFile("./PIPE", "")
  :: Stream (Float * Float * Float * Int16 * Int16 * Int16));

  /*
time = chans.(time) `window(512);
lat  = chans.(lat)  `window(512);
long = chans.(long) `window(512);
x    = sm(int16ToFloat, chans.(x)) `window(512);
y    = sm(int16ToFloat, chans.(y)) `window(512);
z    = sm(int16ToFloat, chans.(z)) `window(512);
  */
// assuming sample rate is 380 hz
//z3 = fft_filter(z,notch_filter(1025,150*2,260*2));


//profile :: ((Stream (Sigseg Float)), (Array Complex), Int) -> (Stream (Float * (Sigseg Float)));

//profile :: (Stream (Sigseg (Int16 * Float * Float * Float)), (Array Complex), Int)
//        -> (Stream (Float * (Sigseg Float)));
fun profile(S, profile, skip) {
  len    = 2*(Array:length(profile)-1);
  window = gaussian(skip`i2f, len);
  rw     = rewindow(S, len, skip-len);
  iterate win in rw {

    arr  = toArray $ sigseg_map(fun((accel, t,lat,lon)) accel`int16ToFloat, win);
    arr2 = apairmult(profile, fftR2C(apairmult(arr,window)));

    let sum = Array:fold(fun (acc, x) acc + absC(x),
    			 gint(0), arr2);
    emit(sum, win);
  }
}

notch1 = notch_filter(129,58,128);
notch2 = notch_filter(129,37,65);

xw = profile(chans.(x, time,lat,long) `window(512), notch1,64);
yw = profile(chans.(y, time,lat,long) `window(512), notch1,64);
zw = profile(chans.(z, time,lat,long) `window(512), notch2,64);

totalscore = iterate(((x,wx),(y,wy),(z,wz)) in zip3_sametype(xw,yw,zw)) {
  println("@@ " ++ x ++ " " ++ y ++ " " ++ z ++ " " ++ x+y+z);
  emit(x+y+z,wz);
};


dets :: Stream (Bool * Int * Int * Int * Float * Float * Sigseg t);
dets = detect(totalscore);

//dets as (b,st,en, l,p,i) = detect(totalscore);

//========================================
// Synchronization:

final :: Stream (Float * Float * Float * Int * Float * Float);
final = iterate (b,st,en, pk,mx,i, win) in dets {
  // This gives an out-of-bounds error!:
  //let (_, time,lat,lon) :: (Int16 * Float * Float * Float) = win[[pk]];
  // So does this!
  // let (_, time,lat,lon) :: (Int16 * Float * Float * Float) = win[[pk - win`start]];
  let (_, time,lat,lon) :: (Int16 * Float * Float * Float) = win[[0]];
 

  /*
    lat  = ref(segs,1);
  long = ref(segs,2);
  x    = ref(segs,3);
  y    = ref(segs,4);
  z    = ref(segs,5);
  index = l - time.start;
  println("@@@ "++time[[index]]++" "++lat[[index]]++" "++long[[index]]
	  ++" "++p++" "++i);
  for i = 0 to time.width-1 {
    println("@$@ "++time[[i]]++" "++lat[[i]]++" "++long[[i]]
	    ++x[[i]]++" "++y[[i]]++" "++z[[i]]);
  }
  */

  if b then emit (time,lat,lon, pk,mx,i);
  //if b then emit (pk,mx,i);
}


// For 5 tuples... xw/yw/zw take 350 ms each... But the zip takes 3000 ms!

BASE <- 
//chans
//x
//xw
//unionList([xw,yw,zw])
//unionList([xw,yw])
//CONST(notch_filter(129,58,128))
//yw
//zip3_sametype(xw,xw,xw)
//zip3_sametype(xw,yw,zw)
//totalscore
//dets
final


// wsc: Worked with rev 1342 of the engine
