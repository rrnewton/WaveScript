
//include "stdlib.ws";

using TOS;
namespace Node {
  bufsize = 2000; 
  rate = 8000.0;
  src1 = read_telos_audio(bufsize, rate);
  s2 = iterate arr in src1 {
    len = Array:length(arr);
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
      print(arr[len] ++ " \tsum: ");      

      sum = Array:fold(fun(acc,n) acc + (cast_num(n)::Int32), (0::Int32), arr);
      print((sum) ++ "  avg: ");
      print((sum / (cast_num(len)::Int32)) ++ "\n");
    };
    emit len;
  }
}

main = Node:s2;
