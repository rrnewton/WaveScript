
// test for subscription

include "netsource.ws";

s1 = netsub_int("breeze", "test");

s2 = iterate f in s1 {
  print("got counter from breeze " ++ f ++ "\n");
};

BASE <- s2;
