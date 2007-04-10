
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

fun thresh_extract(search,streamlist,thresh,pad) {

  ctrl = iterate (w in search) {
    state {
      skiptill = pad;
    }

    //println("in thresh, w.start " ++ w.start);

    for i = 0 to w.width-1 {
      if ((w.start+i) > skiptill && absF(w[[i]]) > thresh) then {
        //println("about to emit t, " ++ w.start + i - pad ++ " " ++ w.start + i + pad - 1);
    	emit (true, w.start + i - pad, w.start + i + pad - 1);
        skiptill := w.start + i + pad;
      }
    };
    
    if (w.start+w.width-pad-1 > skiptill) then {
      //println("about to emit f, " ++ w.start + w.width - pad - 1);
      emit (false, 0, w.start + w.width - pad - 1);
    }
  };

  syncN(ctrl,streamlist);
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


sm = stream_map;

// ============================================================
// Main query:

// This query reads in:
//   (Timestamp, Lat, Long, X,Y,Z)
// Where X/Y/Z is accelerometer data.
//
chans = (readFile("/tmp/clip", "")
//chans = (readFile("/dev/stdin", "")
//chans = (readFile("./PIPE", "")
          :: Stream (Float * Float * Float * Int16 * Int16 * Int16));

x = window(sm(fun((_,_,_,a,_,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,_,_,a,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,_,_,_,_,a)) int16ToFloat(a), chans), 512);


// assuming sample rate is 380 hz
//z3 = fft_filter(z,notch_filter(1025,150*2,260*2));


profile :: ((Stream (Sigseg Float)), (Array Complex), Int) -> (Stream Float);
fun profile(s,profile,skip) {
  len = 2*(Array:length(profile)-1);
  window = gaussian(intToFloat(skip),len);
  rw = rewindow(s, len, skip - len);
  iterate win in rw {

    arr = toArray(win);
    arr2 = apairmult(profile,fftR2C(apairmult(arr,window)));

    let (_,sum) = Array:fold(fun ((i,acc), x) 
			     (i+1, acc + absC(x)),
			     (0,gint(0)), arr2);
    emit(sum);

    //    emit(absC(adot(profile,fftR2C(apairmult(arr,window)))));
  }
}


notch1 = notch_filter(129,58,128);
notch2 = notch_filter(129,37,65);

xw = profile(x,notch1,64);
yw = profile(y,notch1,64);
zw = profile(z,notch2,64);


totalscore = iterate((x,y,z) in zip3_sametype(xw,yw,zw)) {
  println("@@ " ++ x ++ " " ++ y ++ " " ++ z ++ " " ++ x+y+z);
  emit(x+y+z);
};


// For 5 tuples... xw/yw/zw take 350 ms each... But the zip takes 3000 ms!

BASE <- 
//chans
//x
//xw
//CONST(notch_filter(129,58,128))
//yw
//zip3_sametype(xw,xw,xw)
//zip3_sametype(xw,yw,zw)
totalscore


// wsc: Worked with rev 1342 of the engine
