


/* 
 * This runs a network test by gradually increasing the message rate.
 *
 * 
 * .author Ryan Newton
 *
 */

/* 
 * [2008.04.11]
 * Jeez, it seems like every time I turn around everything stops working on TinyOS. *
 *
 * It's not working well in non-WSRADIOMODE atm.  And perhaps the
 * RADIOMODE ver is getting hosed by stray packets from other nodes
 * programmed some time ago.
 *
 * [2008.09.09] I could add support for irregular periods, which would
 * allow us to milk a little more out of our fixed timers.
 *
*/

/*

Run this with:
   wstiny -split 

 */

using TOS;
using Array;
using Mutable;


// The CTP (collection tree protocol) knocks out 8 bytes...
//toparr :: Array Int16 = Array:build((20 / 2) - 1, fun(i) i.gint)
toparr :: Array Int16 = Array:build(8, fun(i) i.gint + 300)

// Hardware timer rate:
maxrate = 5000 // 200 //512
step = 5 // Step period down by

// Epoch in seconds, this may not fit in 16 bits:
//epoch = 60 * maxrate // One minute
epoch :: Int32 = 120 * maxrate // Two minutes

//mytimer = TOS:timer;
//mytimer = timer;

Node:src = timer$ maxrate
//Node:src = read_telos_audio(14, 100)
//Node:src = read_telos_audio(100, 200)

Node:strm = iterate arr in Node:src {
  state { 
          cur :: Int16 = 0;       // our counter
	  nextlvl :: Int32 = 0;  // A separate counter for advancing the epoch.

	  // The size of the wait between firings (which decreases):
          cap :: Int16 = maxrate; // Start off w/ one msg/sec
	  // Every epoch we change rate:
	  epochnum :: Int16 = 0;   // Count elapsed epochs
	  msgcounter :: Int16 = 0; // Count sent messages

	  // NEW SYSTEM:
	  // Increment the rate (in msgs per second), and then map that onto period:
	  rate = 1
	  }

  // Just die at the end:
  if cap < 1 then wserror("alldone");

  //led0Toggle();
  led1Toggle();
  //led2Toggle();
  let id = getID();

  // Due to very limited marshaling interface from tinyOS, we pass back only an array of numbers.

  // Enough ticks have gone by that we're ready to fire:
  if cur == cap then {
    cur := 0;
    msgcounter += 1;

    //payload = maxPayloadLength(); // 28 by default.
    let dropped = getDroppedInputCount();

    toparr[0] := Int16! id;
    toparr[1] := cur;
    toparr[2] := cap;
    //toparr[3] := nextlvl;
    toparr[4] := epochnum;
    toparr[5] := msgcounter;
    toparr[6] := maxrate;
    toparr[7] := Int16! dropped;    

    // If we're not the base station, then send it off.    
    if id != 1 then emit toparr;
  };

  // If we get to the next round we change the timing.
  if nextlvl == epoch then {
    nextlvl := 0;
    msgcounter := 0;

    // This is painfully slow, but we do a floating point multiply to
    // increase the *rate* by 5% (which means dividing the period by 1.05)
    //
    // This will explore from 1hz to 150hz in ~100 epochs, with a
    // 20Khz timer.  (The integer truncation makes cap reduce faster.)
    // With a 5Khz timer, it get's closer to ~200hz.
    newcap = Int16! (Float! cap * 0.9523809523809523);
    // Cheaper would be cap - (cap >> 4)
    // But that wouldn't work at small numbers...
    // Could make the rounding behavior better by doing ((int32_t)cap * 15) >> 4
    // But it stillwon't work at real small numbers.

    // We had better reduce the cap by at least one!
    if newcap == cap  
    then cap := cap - 1
    else cap := newcap;

    if cur > cap then cur := 0;
    //println$ "Changed cap "++cap;

    epochnum += 1;
    //if id != 1 then print("New epoch "++epochnum++" cap is "++cap++"\n");
  };

  nextlvl += 1;  // Count ticks towards the end of the epoch
  cur += 1;      // Count ticks towards firing off a message
}


//====================================================================================================
// On the server side we need to tally the messages.

main = iterate arr in Node:strm {
  state { 
    dropped_network :: Int64 = 0;
    last_msg   = -1;
    last_epoch = -1;        
  }

  id  = arr[0];
  cur = arr[1];
  cap = arr[2];
  epochnum   = arr[4];
  msgcounter = arr[5];
  maxrate    = arr[6];
  dropped_in = arr[7];

  /*
  // Figure out if we've missed messages.
  if epochnum > last_epoch then {
    
  }
  else if msg_num > last_msg + 1 then {
    
  };
  */  

  // Here we just print out the messages received and compute message losses after the fact.
  //println(arr);
  //println((id, msgcounter, epochnum, cur,cap, maxrate, dropped_in));
  //emit ();  
  print( id ++" "++ msgcounter ++" "++ epochnum ++" "++ cap ++" "++ maxrate ++" "++ dropped_in ++"\n");
};
