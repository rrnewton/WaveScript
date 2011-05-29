

// [2009.11.20] This example opens multiple sockets between two different processes.
// It is not supported by my initial implementation of socket.ws

include "socket.ws"

port = 9700;
port2 = 9701;

fun COUNTUP(rate, n)
  iterate _ in timer(rate) {
    // Should be Int64:
    state { counter = n }
    emit (counter);
    counter := (counter) + 1;
  }

// Sender:
//========================================

// First build a stream of some kind of data object:
nums = iterate n in COUNTUP(10, 10) {
  x = (NAME="hello"++n, DAT= (['a','b'], #[n,n+1]));
  emit x;
};

out1 = socket_out(nums, port);
out2 = socket_out(COUNTUP(10, (100::Int)), port2);

main1 = merge(out1, out2);


// Receiver: 
// (This is a totally separate program that could be in a separate file.)
//========================================
// This is optional; it just defines an alias -- a shorter way of writing the type:
type MySchema = (| NAME : String, DAT :  (List Char * Array Int));
  
// This needs the type annotation to deserialize:
in1 :: Stream MySchema = socket_in("localhost", port);
in2 :: Stream Int      = socket_in("localhost", port2);

main2 = union2(in1,in2)
