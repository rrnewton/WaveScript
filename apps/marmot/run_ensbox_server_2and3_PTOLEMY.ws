
//include "run_ensbox_2and3.ws";
include "run_ensbox_2and3_LIST.ws";
include "ptolemy.ws";

/*
fun ensbox_to_ptolemy(name, chan, node, time, data) {
  write_to_fifo
    ("/tmp/ptolemy",
     alist_to_ptolemy
     ( [ ("name",name), 
	 ("chan", show(chan)), 
	 ("node", show(node)),
	 ("timestamp", show(time)),
	 ("data", array_to_ptolemy(data)) ] )++"\n")
}
*/

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


/*


  data = List:toArray([ 1,2,3,4,5,6,7,8,9 ]);



fun ensbox_to_ptolemy(name, chan, node, time, data) {
  write_to_fifo
    ("/tmp/ptolemy",
     alist_to_ptolemy
     ( [ ("name",name), 
	 ("chan", show(chan)), 
	 ("node", show(node)),
	 ("timestamp", show(time)),
	 ("data", array_to_ptolemy(data)) ] )++"\n")
}



// fifo,string 
c_write_to_fifo :: (String, String) -> Int =
  foreign("write_string_to_fifo", []);


fun write_to_fifo(fifo, str) {
  c_write_to_fifo(fifo++String:implode([intToChar(0)]), 
		  str++String:implode([intToChar(0)]))
}


fun array_to_ptolemy(a) {
  if (a`Array:length == 0) 
    then "{}"
      else  
	foldRange(0, a`Array:length-2, "{", fun (x,y) x++a[y]++",")++
	  a[a`Array:length-1]++"}";
}

fun assoc_to_ptolemy((x,y)) {
  x++"="++y
}

fun alist_to_ptolemy(al) {
  if (al`List:length == 0) 
  then "{}"
    else
      "{"++assoc_to_ptolemy(al`head)++
	List:fold1(fun (x,y) ","++y++","++x,
		   List:map(assoc_to_ptolemy,al`tail))++"}";
}



*/
