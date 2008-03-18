
/* 
 * This is a nonsense program where I'm testing the compilation of
 * different features under TinyOS. 
 *
 * .author Ryan Newton
 *
 */

include "stdlib.ws";

commbuf = Array:make(10, 0);

// If events occur at the same time... that's a problem.
// They collide in the shared buffer space used to cummunicate tuples.

using Mutable;
using TOS;
namespace Node {

  src0 = (readFile("profile.dat", "", timer(2.0)) :: Stream Uint16);
  src1 = sensor_uint16("DemoSensorC", 2.0);

  s1 = iterate reading in IFPROFILE(src0, src1) { 
    emit reading;
  };

  s2 = iterate reading in s1 {
    state { cntr = 0 }
    commbuf[0] := reading;
    for i = 1 to 4 {
      commbuf[i] := (cast_num(i) :: Uint16);
    }
    //print("length: "++ Array:length(commbuf) ++"\n");
    commbuf[Array:length(commbuf)-1] := cntr;
    cntr += 1;
    emit commbuf;
  }
  ;//.merge(load_telos32khzCounter); // Ugly way to link in a component.

  s3 = iterate(buf in s2) {
    state { lasttime = 0 }

    //led2Toggle();    
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
    for i = 1 to 1000 { sum += 1; }
    emit (x,sum);
  }
}

//s5 = iterate _ in timer$ 0.66 { emit (0,1); }

//main = merge(s5, Node:s3);

s5 = iterate ((x,y),z) in Node:s4 {

    ind :: Ref Int64 = ref(3333);    
    for i = 1 to 100 { ind += 1 };
    //while ind < 1 * 1000 * 1000 { ind += 1 };

    //println("I'M ON SERVER "++y);
    //println("I'M ON NODE ");
    emit (x,y+1,z)
}

s6 = iterate x in s5 {
    ind = ref(0);
    for i = 1 to 1000 { for j = 1 to 1000 { ind += 1 } };
    emit (x,ind);
}

main = s6;
//main = Node:s2;
