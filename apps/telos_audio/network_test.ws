



/* 
 * This simply reports the node IDs of every node.
 *
 * .author Ryan Newton
 *
 */

/* 
 * [2008.04.11]
 * Jeez, it seems like every time I turn around everything stops working on TinyOS. *

 * It's not working well in non-WSRADIOMODE atm.  And perhaps the
 * RADIOMODE ver is getting hosed by stray packets from other nodes
 * programmed some time ago.
 *
 */

using TOS;

//Node:strm = iterate arr in read_telos_audio(200, 100) {

Node:strm = iterate arr in timer$ 1.0  {
  //state { cnt = 0 }

  led0Toggle();
  led1Toggle();
  led2Toggle();

/*
  cnt += 1;
  id = getID();

  //print("max payload "++ maxPayloadLength() ++"\n");
  //payload = maxPayloadLength();



  //if id != 1 then print("Running on "++id++" cnt "++cnt++"\n");
  //emit (234, id, cnt, payload);
  //emit cnt;
  //emit payload;
  //emit id;
*/

  emit 234;

}

main = Node:strm;
