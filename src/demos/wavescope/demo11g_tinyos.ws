

include "stdlib.ws";

commbuf = Array:make(5, 0);

// If events occur at the same time... that's a problem.
// They collide in the shared buffer space used to cummunicate tuples.

namespace Node {
  s2 = iterate( reading in sensor_uint16("DemoSensorC", 1.0) ) {
    //println("Got sensor reading...");
    commbuf[0] := reading;
    //led0Toggle();
    //led1Toggle();
    emit commbuf;
   };

  s3 = iterate(buf in s2) {
    //led2Toggle();
    //println(buf);
    //emit (Array:fold((+), 0, buf), false);
    emit (Array:fold((+), 0, buf), 515);
  };

  //s4 = iterate _ in timer$2 { led0Toggle(); };
}



//s5 = iterate _ in timer$ 0.66 { emit Array:null; }
//s5 = iterate _ in timer$ 0.66 { emit (0,true); }
s5 = iterate _ in timer$ 0.66 { emit (0,1); }

//main = s5.merge(Node:s4).merge(Node:s3);

//main = merge(s5, Node:s3);
main = smap(fun((x,y)) {
    //println("IM ON SERVER "++y);
    //println("IM ON NODE ");
    (x,y+1)
  }, Node:s3);
