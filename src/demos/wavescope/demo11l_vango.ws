

include "stdlib.ws";

using Mutable;
using TOS;
namespace Node {
  bufsize = 1024; // works (readstream tolerates 1000 us sample period)
  //bufsize = 100; // works
  //bufsize = 32;  // Works 
  //bufsize = 30; // Breaks
  //rate = intToFloat(bufsize) / 2.0;
  //rate = 8000.0;
  rate = 8000.0;
  //src1 = readstream_uint16("DemoSensorStreamC", bufsize, rate);
  src1 = read_telos_audio(bufsize, rate);
  s2 = iterate arr in src1 {
    len = Array:length(arr);
    emit arr;
  }

  s3 = iterate arr in s2 {
    state {
      mean :: Int32 = 0
    }

    sum = Array:fold(fun(acc,n) acc + (cast_num(n) :: Int32), (0::Int32), arr);

    //sum = rshiftI32(sum,10);

    av = sum / 1024;

    if (mean == 0) then {
      mean := av;
    };

    // exponential moving average with correct rounding
    // (otherwise there is a drift down).
    tmp = (255*mean + av);
    newmean = (255*mean + av) / 256;
    if ( tmp - newmean * 256 > 127 ) then {
      mean := newmean + 1;
    } else {
      mean := newmean;     
    };

    len = Array:length(arr);

    zero_crossings = Mutable:ref(0);

    mean16 = (cast_num(mean) :: Uint16);
    for i = 1 to len-1 {
      //a1 = (cast_num(arr[i]) :: Int32);
      //if ( a1 > mean ) then {
      if (   ( arr[i] > mean16 && arr[i-1] < mean16 ) 
          || ( arr[i] < mean16 && arr[i-1] > mean16 ) ) then {
        zero_crossings := zero_crossings + 1;
      };
    };

    zc = zero_crossings;
    print("zc = " ++ zero_crossings ++ ", ");


    emit len;

    //println(arr);
    if true then {
      print(mean ++ "/" ++ av ++ "/");
      print(arr[0] ++ " ");
      print(arr[1] ++ " ... ");
      print(arr[len-2] ++ " ");
      print(arr[len-1] ++ " | ");
      print(arr[len] ++ " \tsum: ");      
      //sum :: Ref Int64 = ref(0);
      //Array:foreach(fun(n) sum += intToInt64(n), arr);
      //sum = Array:fold((+), (0::Int64), arr);
      //sum = Array:fold(fun(acc,n) acc + intToInt64(n), (0), arr);
      //print(sum / intToInt64(len) ++ "\n");

      /*
      print(floatToInt64(sum) ++ "  avg: ");
      print(floatToInt64(sum / intToFloat(len)) ++ "\n");
      */

      
      led2Toggle();
      
      print((sum) ++ "  avg: ");
      print((sum / (cast_num(len) :: Int32)) ++ " ");

      print("\n");

    };    
    //emit len;
  }
}

main = Node:s3;
