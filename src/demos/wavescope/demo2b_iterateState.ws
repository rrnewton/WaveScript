








// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 40", timer(1000.0 / 40.0)) :: Stream (Sigseg Int16));
Node:s1 = timer(100);

//counter = 99;

// Identity iterate.
Node:s2 = iterate( w in Node:s1 ) {
  //  state{ static counter = 0 }
  //  counter := static(statref(counter) + 1);
  state{ counter :: Int = 0 }
  //state{ counter = 99 } // [2007.10.02] It will default to int....
  counter += 1;

  //led0Toggle(); led1Toggle(); led2Toggle();

  //print("why?\n");
  //println(clock());
  
  if counter == 100 then { 
    //wserror("foo");
    //print("\nSTARTING:\n");
  }; 
  emit counter;
};

main = Node:s2;
