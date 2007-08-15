

include "run_ensbox_server_2and3.ws";
include "ptolemy.ws";

allsynced = 
  List:fold1(merge,
             map(fun((node,strm))
                 stream_map(fun(x) (node,x), strm),
		 List:zip(nodes,floats)))

ptolemized = iterate sum in union3(final, allsynced, merged) {

  case sum {
    Oneof3(_): {
      println("GOT HEATMAP");      
    }

    Twoof3(tup): {
      let (nd,det) = tup;
      println("GOT DETECTION\n");
      /*
      data = toArray$ List:ref(det,0);
      str = alist_to_ptolemy(
       //[ ("name","\"detection\""), 
        [ ("name","44444"), 
	  ("node", "103"), 
	  ("chan", "0"),
	  ("timestamp", "0"),
	  ("data", array_to_ptolemy(data) ) ]);
      write_to_fifo("/tmp/ptolemy_dets", str++"\n");
      println("WROTE TO PTOLEMY VIA PIPE!!!!!!!!");
      */
    }

    Threeof3(tup): {
      let (noderec, start, amlvec) = tup;
      println("GOT AML\n");

      data = amlvec;
      str = alist_to_ptolemy(
       //[ ("name","\"detection\""), 
        [ ("name","11111"), 
	  ("node", "103"), 
	  ("chan", "0"),
	  ("timestamp", show(start)),
	  ("data", array_to_ptolemy(data) ) ]);
      write_to_fifo("/tmp/ptolemy_dets", str++"\n");
      println("WROTE AML TO PTOLEMY VIA PIPE!!!!!!!!");
    }
  }
};


BASE <- ptolemized
