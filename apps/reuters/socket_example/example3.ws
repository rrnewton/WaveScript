

// [2009.11.20] This example opens multiple sockets between two different processes.
// It is not supported by my initial implementation of socket.ws

include "socket.ws"

port = 9700;
port2 = 9701;


// Sender:
//========================================

// First build a stream of some kind of data object:
nums = iterate n in COUNTUP(10) {
  x = (NAME="hello"++n, DAT= (['a','b'], #[n,n+1]));
  print("Sending: "++ x  ++"\n"); 
  emit x;
};

out_first = socket_out(nums, port);
and_back :: Stream Int = socket_in("localhost", port2)

main1 = merge(out_first, and_back)


// Receiver: 
// (This is a totally separate program that could be in a separate file.)
//========================================
// This is optional; it just defines an alias -- a shorter way of writing the type:
type MySchema = (| NAME : String, DAT :  (List Char * Array Int));
  
// This needs the type annotation to deserialize:
instrm :: Stream MySchema = socket_in("localhost", port);

results = iterate x in instrm { emit String:length(x.NAME) }

main2 = socket_out(results, port2)

