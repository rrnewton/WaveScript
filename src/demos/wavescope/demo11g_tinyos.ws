
/* 
 * This is a nonsense program where I'm testing the compilation of
 * different features under TinyOS. 
 *
 * .author Ryan Newton
 *
 */

include "stdlib.ws";

commbuf = Array:make(5, 0);

// If events occur at the same time... that's a problem.
// They collide in the shared buffer space used to cummunicate tuples.

namespace Node {
  s2 = iterate( reading in sensor_uint16("DemoSensorC", 1.0) ) {
    //println("Got sensor reading...");
    commbuf[0] := reading;
    emit commbuf;
   };

  s3 = iterate(buf in s2) {
    led2Toggle();
    //println(buf);
    emit (Array:fold((+), 0, buf), 515);
  };
}

s5 = iterate _ in timer$ 0.66 { emit (0,1); }

//main = merge(s5, Node:s3);

main = smap(fun((x,y)) {
    //println("I'M ON SERVER "++y);
    //println("I'M ON NODE ");
    (x,y+1)
  }, Node:s3);
