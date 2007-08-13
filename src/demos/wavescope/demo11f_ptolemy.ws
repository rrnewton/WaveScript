
include "stdlib.ws";
include "ptolemy.ws";


BASE <- iterate x in timer(3.0) {

data = listToArray([ 1,2,3,4,5,6,7,8,9 ]);

write_to_fifo("/tmp/ptolemy", "TEST1\n");
write_to_fifo("/tmp/ptolemy", 
alist_to_ptolemy(
[ ("tag","hello"), ("node", 1), 
  ("data", array_to_ptolemy(data) ) ] ));

};