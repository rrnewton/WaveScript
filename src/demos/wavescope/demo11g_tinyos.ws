
/* 
 * This is a nonsense program where I'm testing the compilation of
 * different features under TinyOS. 
 *
 * .author Ryan Newton
 *
 */

include "stdlib.ws";

bufsize = 520;

//type BufTy = Float;
type BufTy = Uint16;
//type BufTy = Int32;

commbuf :: Array BufTy = Array:make(bufsize, 0);

// If events occur at the same time... that's a problem.
// They collide in the shared buffer space used to cummunicate tuples.

// This needs to be constrained to the server so it doesn't get rewritten as a TOS:timer!
src0 = (readFile("profile.dat", "", Server:timer(2.0)) :: Stream Uint16);

using Mutable;
using TOS;
namespace Node {

  src1 = sensor_uint16("DemoSensorC", 2.0);
  //src1 = src0;

  src = IFPROFILE(src0, src1);
  //src :: Stream Uint16 = COUNTUP(0);

  echosrc = iterate reading in src { 
    //print("  got reading...\n");
    emit reading + reading;  // Dissassembling just a multiply.
  };

  bufstrm = iterate reading in echosrc {
    state { cntr = 0; }
    commbuf[0] := (cast_num(reading) :: BufTy);
    for i = 0 to Array:length(commbuf) - 1 {
      commbuf[i] := (cast_num(i) :: BufTy);
    }
    //print("length: "++ Array:length(commbuf) ++"\n");
    commbuf[Array:length(commbuf)-1] := cntr;
    cntr += 1;
    emit commbuf;
  }
  ;//.merge(load_telos32khzCounter); // Ugly way to link in a component.
  
  // Do a time-consuming multiply-and-add for every floating point element.
  muladd = iterate(buf in bufstrm) {
    state { sum = 0 }

    //start = clock();

    // Do ~4164 multiple-adds, models eugene's app for one channel:
    for i = 1 to 8 { // 800000 for pc
      sum += Array:fold((fun(acc,n) (acc+n) * n), 0, buf);
    };

    //println("elapsed "++clock() - start);
    
    emit (sum, getID());
  };

  sillyforloop = iterate x in muladd {
    //state { sum :: Int64 = 0 } sum := 0;
    sum = Mutable:ref$ (0::Int64);
    for i = 1 to 1000 { sum += 1; }
    emit (x,sum);
  }
}

//s5 = iterate _ in timer$ 0.66 { emit (0,1); }

//main = merge(s5, Node:s3);

s5 = iterate ((x,y),z) in Node:sillyforloop {
    ind :: Ref Int64 = ref(3333);    
    for i = 1 to 100 { ind += 1 };
    //while ind < 1 * 1000 * 1000 { ind += 1 };

    //println("I'M ON SERVER "++y);
    //print("I'M ON NODE " ); println(y); 
    emit (x,y,z+1)
}

s6 = iterate x in s5 {
    ind = ref(0);
    for i = 1 to 1000 { for j = 1 to 1000 { ind += 1 } };
    //print(" Almost done: "); println(x);
    emit (x,ind);
}

main = s6;
//main = Node:muladd;


/*

ODD, hardware multiply-adds are SLOWER on msp430.
I hacked telosb.target to not include the -mdisable-hwmul flag.

This is the code for HW mul:

   630c:    02 12           push    r2        ;
   630e:    32 c2           dint            
   6310:    03 43           nop            
   6312:    92 44 02 00     mov    2(r4),    &0x0132    ;
   6316:    32 01 
   6318:    92 44 02 00     mov    2(r4),    &0x0138    ;
   631c:    38 01 
   631e:    94 42 3a 01     mov    &0x013a,4(r4)    ;0x013a
   6322:    04 00 
   6324:    32 41           pop    r2        ;

This is the code for SW mul:

   630c:    1a 44 02 00     mov    2(r4),    r10    ;
   6310:    1c 44 02 00     mov    2(r4),    r12    ;
   6314:    b0 12 f6 d9     call    #-9738        ;#0xd9f6
   6318:    84 4e 04 00     mov    r14,    4(r4)    ;


Cycles / muladd:

hwmul int16 55
swmul int16 223

hwmul int32 132
swmul int32 243

float 32bit 1923

cycles per int16 addadd: 22

Note: on honor (intel x5482), "addadd" takes avg 1.15 cycles, muladd takes 4.0... why 4.0?

*/
