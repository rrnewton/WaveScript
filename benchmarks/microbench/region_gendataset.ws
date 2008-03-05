

include "stdlib.ws"
include "unix.ws"




using Curry;
//s1 = smap(show) $ smap(cos) $ COUNTUP(0);

main = {
  SHELL("rm -rf dat; mkdir dat");
  //fileSink("dat/1.dat", "w", s1);
  COUNTUP(10);
}


main = COUNTUP(10);
