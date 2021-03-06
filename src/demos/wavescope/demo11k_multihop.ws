



/* 
 * This simply reports the node IDs of every node.
 *
 * .author Ryan Newton
 *
 */

// On the base station we use the serial for returning the result
// stream.  But on the other nodes we are free to print messages to
// the serial port if we like.
Node:ids = iterate _ in timer(1) {
  state { cnt = 0 }
  //cnt += 1;
  id = getID();
  //if id != 1 then print("Running on "++id++" cnt "++cnt++"\n");
  //emit (id, cnt);
  emit id;
}

main = Node:ids;
