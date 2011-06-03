

// [2011.05.31] A small tweak of example3.ws that measures data rates of its multiple input streams.

include "socket.ws"

port = 9700;
port2 = 9701;

fun COUNT_UP(rate, n)
  iterate _ in timer(rate) {
    // Should be Int64:
    state { counter = n }
    emit (counter);
    counter := (counter) + 1;
  }

// Sender:
//========================================

// First build a stream of some kind of data object:
nums = iterate n in COUNT_UP(15, 10) {
  x = (NAME="hello"++n, DAT= (['a','b'], #[n,n+1]));
  emit x;
};

out1 = socket_out(nums, port);
out2 = socket_out(COUNT_UP(5, (100::Int)), port2);

main1 = merge(out1, out2);


// Receiver: 
// (This is a totally separate program that could be in a separate file.)
//========================================
// This is optional; it just defines an alias -- a shorter way of writing the type:
type MySchema = (| NAME : String, DAT :  (List Char * Array Int));
  
// This needs the type annotation to deserialize:
in1 :: Stream MySchema = socket_in("localhost", port);
in2 :: Stream Int      = socket_in("localhost", port2);

inputs = union2(in1,in2)
heartbeat = timer(1.0) // 1hz

main2 = iterate x in union2(heartbeat, inputs) {
  state { 
           seconds = 0; 
           left    = 0;
           right   = 0;
        }
  case x {
    Left(_): { 
                seconds += 1;
                print("After "++seconds++" seconds, got "++left++" / "++right++" on left/right.\n");
             }
    Right(inp): case inp { 
                  Left(_):  left += 1
                  Right(_): right += 1
                }
  }
}
