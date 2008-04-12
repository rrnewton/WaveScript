



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
using Array;
using Mutable;

//Node:strm = iterate arr in read_telos_audio(200, 100) {


// The CTP knocks out 8 bytes...
//toparr :: Array Int16 = Array:build((20 / 2) - 1, fun(i) i.gint)
toparr :: Array Int16 = Array:build(9, fun(i) i.gint)  

zeroarr :: Array Int16 = Array:make(0, 0)
//zeroarr :: Array Int16 = Array:null

Node:src = TOS:timer$ 10.0;
//Node:src = read_telos_audio(14, 100)
//Node:src = read_telos_audio(100, 200)

epoch = 200

Node:strm = iterate arr in Node:src  {
  state { nextlvl :: Int16 = 0;
          cur :: Int16 = 0;
          cap :: Int16 = 10;
	  epochnum :: Int16 = 0;
	  }

  led0Toggle();
  led1Toggle();
  led2Toggle();
  id = getID();
  
  //arr[0] := cnt;
  toparr[0] := cast_num(id);
  toparr[1] := cur;
  toparr[2] := cap;
  toparr[3] := nextlvl;
  toparr[4] := epochnum;

  foo = ref(false);

  if cur == cap then {
    cur := 0;
    if id != 1 then print(".");
    //if id != 1 then emit toparr;
    foo := true;
  };

  //if id != 1 && foo then emit toparr;
  if id != 1 then emit toparr else emit zeroarr;

  if nextlvl == epoch then {
    nextlvl := 0;
    cap := cap - 1;
    epochnum += 1;
    if id != 1 then print("New epoch "++epochnum++" cap is "++cap++"\n");
  };

  nextlvl += 1;
  cur += 1;
  
  //if id > 1 then emit toparr;

  //payload = maxPayloadLength(); // 28 by default.
  //dropped = getDroppedInputCount();

  //if id != 1 then print("Running on "++id++" cnt "++cnt++"\n");
  //if id != 1 then print("arr "++toparr++"\n");

  //emit arr;
  //emit arr;
  //emit (payload, 234, id, cnt, dropped);

  //x :: Int32 = 99;
  //emit ((cast_num(id)::Int32), cnt, x,x,x,x,x);
  //emit(x,x,x,x,x)
  //emit 8587;
  //emit (id, toparr.length);  
  
}

main = Node:strm;
