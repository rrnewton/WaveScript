

// The root node must use it's serial port to communicate results from
// the network (as a radio bridge).  But the other nodes should be
// able to send debug info to their serial channels.

using TOS;

Node:s1 = iterate _ in timer(1) {
  state { chr = 65 }


  chr += 1;
  if chr > 90 then chr := 65;

  //serial0_put(65);
  //serial1_put(chr);

  //print("Put byte\n");  
  print("yay ");

  led1Toggle();
  emit 
}

main = merge(load_SerialByteComm, Node:s1)
