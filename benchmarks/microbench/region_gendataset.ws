

include "stdlib.ws"
include "unix.ws"




using Curry;
s1 = smap(show) $ smap(cos) $ COUNTUP(0);

sunk = fileSink("dat/1.dat", "w", s1);

main = {
  SHELL("rm -rf dat; mkdir dat");  
  //COUNTUP((11.99::Float));
  sunk//s1
}


//main = COUNTUP(10);
