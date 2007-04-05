
include "stdlib.ws";
include "filter.ws";
//include "matrix.ws";

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

chans = (readFile("/tmp/crap", "")
          :: Stream (Float * Float * Float * int16 * int16 * int16));

x = window(sm(fun((_,_,_,a,_,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,_,_,a,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,_,_,_,_,a)) int16ToFloat(a), chans), 512);


// assuming sample rate is 380 hz
//z3 = fft_filter(z,notch_filter(1025,150*2,260*2));



fun profile(s,profile,skip) {
  fw = fftStream(rewindow(s,Array:length(profile)-1,skip-(Array:length(profile)-1)));
  iterate (win in fw) {
    state {
      sum = 0.0+0.0i;
    }
    sum := 0.0+0.0i;
    a = toArray(win);
    for i = 0 to Array:length(profile)-1 {
      sum := sum + (profile[i] * a[i]);
    };
    absC(sum)
    //    emit(absC(adot(profile,toArray(win))));
  }
}

xw = profile(x,notch_filter(129,58,128),64);
yw = profile(y,notch_filter(129,58,128),64);
zw = profile(z,notch_filter(129,37,65),64);

totalscore = iterate((x,y,z) in zip3_sametype(xw,yw,zw)) {
  emit(x+y+z);
};



BASE <- snoop("kl ",totalscore);



