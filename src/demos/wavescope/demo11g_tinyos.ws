
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

using TOS;
namespace Node {
  s1 = iterate( reading in sensor_uint16("DemoSensorC", 1.0) ) {
    emit reading;
  };

  s2 = iterate reading in s1 {
    commbuf[0] := reading;
    for i = 1 to 4 {
      commbuf[i] := (cast_num(i) :: Uint16);
    }
    emit commbuf;
  }
  ;//.merge(load_telos32khzCounter); // Ugly way to link in a component.

  s3 = iterate(buf in s2) {
    state { lasttime = 0 }

    led2Toggle();    
    //println(buf);
    //    print("Time is ");
    //    println(clock32khz());
    
    //cur = clock32khz();
    //print("Time is "++ cur ++" diff "++  cur - lasttime ++"\n");
    //lasttime := cur;

    //for i = 1 to 500 { lasttime += 1; }
    emit (Array:fold((+), 0, buf), 515);
  };

  s4 = iterate x in s3 {
    sum = Mutable:ref$ (0::Int64);
    //for i = 1 to 1000 { sum += 1; }
    emit (x,sum);
  }
}

s5 = iterate _ in timer$ 0.66 { emit (0,1); }

//main = merge(s5, Node:s3);


_main = smap(fun(((x,y),z)) {
    //println("I'M ON SERVER "++y);
    //println("I'M ON NODE ");
    (x,y+1,z)
  }, Node:s4);


main = Node:s2;
