

//include "stdlib.ws";

using TOS;
namespace Node {
  bufsize = 1000; // works (readstream tolerates 1000 us sample period)
  //bufsize = 100; // works
  //bufsize = 32;  // Works 
  //bufsize = 30; // Breaks
  rate = intToFloat(bufsize) / 2.0;
  src1 = readstream_uint16("DemoSensorStreamC", bufsize, rate);
  s2 = iterate arr in src1 {
    len = Array:length(arr);
    emit len;
    //println(arr);
    if true
    then {
      // Note, this hackishly does out of bounds accesses!!
      print(arr[-1] ++ " | ");
      print(arr[0] ++ " ");
      print(arr[1] ++ " ");
      print(arr[2] ++ " ");
      print(arr[3] ++ " ... ");
      print(arr[len-2] ++ " ");
      print(arr[len-1] ++ " | ");
      print(arr[len] ++ "\n");
    };
    //emit ();
  }
}

main = Node:s2;
