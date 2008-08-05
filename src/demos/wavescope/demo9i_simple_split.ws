
// This tests wsc2's ability to do a split execution over two similar linux environments.

namespace Node {

  src = iterate _ in timer(3) { state { cnt = 0 } emit cnt; cnt += 1 }

  echosrc = iterate reading in src { 
    print(" client: got timer tick: "++reading++"\n");
    emit reading ;
  };
}

main = iterate x in Node:echosrc {
    print(" server: got msg: "++x++"\n");
    emit ();
}
