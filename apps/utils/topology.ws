
using TOS;

// This needs a script to go with it and extract/visualize the topology.

Node:info = iterate _ in timer(1) {
  state { cnt = 0 }
  cnt += 1;

  led1Toggle();

  if getID() == 1 
  then { if moduloI(cnt,60) == 0 then emit (cnt,1,0,0); }
  else { 
    emit (cnt, getID(), getTreeParent(), getDroppedInputCount());
  }
}

main = Node:info
