

// The root node must use it's serial port to communicate results from
// the network (as a radio bridge).  But the other nodes should be
// able to send debug info to their serial channels.

// [2008.10.05] I got this to work under certain conditions.  But it's
// very spotty.  And I haven't yet learned how to properly configure
// and read from /dev/ttyUSB0 under ubuntu.  (cat and screen don't do
// it for me, and my own attempt at writing a C program hasn't
// helped.)

// [2008.10.05] Ack, and at other times it seems like I only get
// garbage characters (-2?).

using TOS;

Node:s1 = iterate _ in timer(1) {
  state { chr = 65 }


  chr += 1;
  if chr > 90 then chr := 65;

  // Serial1 turns out to be the one that we listen to by default.
  //serial0_put(chr);
  //serial1_put(chr);

  //serial0_put(65);
  serial1_put(65);

  //print("Put byte\n");  
  //print("yay ");

  led1Toggle();
  //emit ();
}

main = merge(load_SerialByteComm, Node:s1)
