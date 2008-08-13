
// This tests wsc2's ability to do a split execution over two similar linux environments.

// [2008.08.11] Currently you can run it with:
//   ./query_client.exe 2>> /dev/stdout | ./query_server.exe 

gethostname :: (Array Char, Int) -> Int = foreign("gethostname", ["unistd.h"]);

namespace Node {

  src = iterate _ in timer(3) { state { cnt = 0 } emit cnt; cnt += 1 }
  
  echosrc = iterate reading in src { 

    host = Array:make(100,'_');
    result = gethostname(host, Array:length(host));
    host_str = String:implode $ Array:toList $ host;

    emit (reading, 
	  List:build(10, fun(i) reading),
          Array:build(10, fun(i) reading),
	  result,
	  host_str
	  );
  };
}

serv = iterate x in Node:echosrc {
    print(" server: got msg: "++x++"\n");
    //print("Tuple : "++(1,2)++"\n");
    emit () //emit (1,2,3);
}

main = serv
//main = iterate _ in serv { emit () }
