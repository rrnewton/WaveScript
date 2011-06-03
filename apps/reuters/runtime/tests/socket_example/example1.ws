

// [2009.06.10] This is a simple example which sends an (arbitrarily
// complex) piece of wavescope data over a socket.

include "socket.ws"


port :: Uint16 = 9702;


// Sender:
//========================================

// First build a stream of some kind of data object:
nums = iterate n in COUNTUP(10) {
  x = (NAME="hello"++n, DAT= (['a','b'], #[n,n+1]));
  //print("Sending: "++ x  ++"\n"); 
  emit x;
};

outstrm = socket_out(nums, port);
//outstrm2 = socket_out2(nums, Int! port);

// Receiver:
// (This is a totally separate program that could be in a separate file.)
//========================================
// This is optional; it just defines an alias -- a shorter way of writing the type:
type MySchema = (| NAME : String, DAT :  (List Char * Array Int));
  
// This needs the type annotation to deserialize:
instrm :: Stream MySchema = socket_in("localhost", Uint16! port);

//========================================
// This is an alternate way of doing things.
// First, receieve the raw bytes:
in2 = socket_in_raw("localhost", port);

// Second, unmarshal them manually:
instrm2 = iterate bytes in in2 {
  print("Received "++ Array:length(bytes)  ++" bytes: "++ bytes ++ "\n");
  x :: MySchema = unmarshal(bytes, 0); // 0 is offset
  print("  Unpacked into: " ++ x ++"\n");
  emit ();
}


