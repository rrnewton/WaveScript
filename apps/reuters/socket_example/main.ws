

include "socket.ws"


port = 9700;


nums = iterate n in COUNTUP(10) { 
  x = ("hello",#[n,n+1]);
  print("Sending: "++ x  ++"\n"); 
  emit x;
};
outstrm = socket_out(nums, port);


// This needs the type annotation to deserialize:
instrm :: Stream (String * Array Int) = socket_in("localhost", port);
