
include "stdlib.ws";
include "filter.ws";
//include "matrix.ws";

//======================================================================

fun specgram(ext) {
  iterate (s in ext) {
	
    state {
      xcount = 0;
    }
    
    skip = 8;
    points = 512;
    
    hw = gaussian(intToFloat(skip),points);
    
    for j = 0 to points-1 {
      println("## " ++ j ++ " " ++ hw[j]);
    };
    
    //specgram the sync'd data 
    
    fun dospec(i,x) {
      a = toArray(s.subseg(s.start+i,points));
      win = apairmult(a,hw);
      f = fftR2C(win);
      for j = 0 to points/2 {
	println("@@ " ++ x ++ " " ++ j ++ " " ++ absC(f[j])
		++ " " ++ s.start+i);
      };
    };
    
    for i = 0 to ((s.width-points) / skip) {
      dospec(i*skip,xcount);
      println("@@ ");
      xcount := xcount + 1;
    };	
    
    for i = 0 to s.width-1 {
      println("@# " ++ s.start+1 ++ " " ++ s[[i]]);
    };
  };
};




sm = stream_map;

// ============================================================
// Main query:

chans = (readFile("/tmp/tospec", "")
//          :: Stream (Float * Float * Float * Float * Float * Float * Float * Float));

//z = window(sm(fun((_,_,_,_,_,_,_,a)) a, chans), 1024);

          :: Stream (Float * Float * Float * Int16 * Int16 * Int16 * Int16 * Float));

z = window(sm(fun((_,_,_,_,_,a,_,_)) int16ToFloat(a), chans), 3800);

/*
BASE <- iterate(w in z) {
  f = fftR2C(toArray(w));
  for j = 0 to 256 {
 println("");
    println("@@ " ++ absC(f[j]));
  };
};
*/

BASE <- specgram(z);


