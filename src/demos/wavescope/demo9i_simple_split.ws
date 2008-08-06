
// This tests wsc2's ability to do a split execution over two similar linux environments.

namespace Node {

  src = iterate _ in timer(3) { state { cnt = 0 } emit cnt; cnt += 1 }
  
  echosrc = iterate reading in src { 
    //print(" client: got timer tick: "++reading++"\n");
    emit //(reading, 
         //List:build(10, fun(i) reading)
         //, Array:build(10, fun(i) reading));
    (9,10,11)
  };
}

serv = iterate x in Node:echosrc {
    print(" server: got msg: "++x++"\n");
    //print("Tuple : "++(1,2)++"\n");
    emit () //emit (1,2,3);
}

main = serv
//main = iterate _ in serv { emit () }
