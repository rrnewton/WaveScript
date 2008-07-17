








// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 40", timer(1000.0 / 40.0)) :: Stream (Sigseg Int16));
s1 = timer(1000);

// Identity iterate.
s2 = iterate( w in s1 ) {
  //  state{ static counter = 0 }
  //  counter := static(statref(counter) + 1);
  //state{ counter :: Int = 0 }
  state{ counter = 99 } // [2007.10.02] It will default to int....
  counter += 1;

  //led0Toggle(); led1Toggle(); led2Toggle();

  //print("why?\n");
  //println(clock());
  
  if counter == 100
  then print("\nSTARTING:\n");
  emit counter;
};

main = s2;
