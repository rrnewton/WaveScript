
// ensbox platform testing
// test for publishing

include "timersource.ws";
include "netsource.ws";

s1 = timer_source("test", 1000);

s2 = iterate f in s1 {
  print("got counter " ++ f ++ "\n");
  emit(f);
};

main = netpub_int(s2, "test");
