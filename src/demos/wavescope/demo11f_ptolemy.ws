
// Test for hooking up to Ptolemy.

include "stdlib.ws";
include "ptolemy.ws";


main = iterate x in timer(3.0) {

data = List:toArray([ 1,2,3,4,5,6,7,8,9 ]);

write_to_fifo("/tmp/ptolemy", "TEST1\n");

str = alist_to_ptolemy(
[ ("tag","hello"), ("node", "1"), 
  ("data", array_to_ptolemy(data) ) ]);

println(str);
write_to_fifo("/tmp/ptolemy", str);

};
