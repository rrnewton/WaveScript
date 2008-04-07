

include "stdlib.ws";

using Mutable;
using TOS;
namespace Node {
  bufsize = 2000; // works (readstream tolerates 1000 us sample period)
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
    len = Array:length(arr);

    //println(arr);
    if true
    then {
      print(arr[0] ++ " ");
      print(arr[1] ++ " ");
      print(arr[2] ++ " ");
      print(arr[3] ++ " ... ");
      print(arr[len-2] ++ " ");
      print(arr[len-1] ++ " | ");
      print(arr[len] ++ " \tsum: ");      
      //sum :: Ref Int64 = ref(0);
      //Array:foreach(fun(n) sum += intToInt64(n), arr);
      //sum = Array:fold((+), (0::Int64), arr);
      //sum = Array:fold(fun(acc,n) acc + intToInt64(n), (0), arr);
      //print(sum / intToInt64(len) ++ "\n");

      /*
      sum = Array:fold(fun(acc,n) acc + (cast_num(n) :: Float), (0.0), arr);
      print(floatToInt64(sum) ++ "  avg: ");
      print(floatToInt64(sum / intToFloat(len)) ++ "\n");
      */

      sum = Array:fold(fun(acc,n) acc + (cast_num(n) :: Int32), (0::Int32), arr);
      
      led2Toggle();
      
      print((sum) ++ "  avg: ");
      print((sum / (cast_num(len) :: Int32)) ++ " ");

      print("\n");

    };    
    //emit len;
  }
}

main = Node:s3;
