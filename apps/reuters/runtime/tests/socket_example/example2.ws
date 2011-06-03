

// [2009.06.10] This example uses sockets in conjunction with wsqlib.ws

include "socket.ws"
include "../../wsqlib.ws"

// Use the fake stock feed from this example:
include "../../../stonebreaker_example_queries.ws"

include "stdlib.ws" 

// Here we do a chain of filters, breaking into separate processes.

// I abstract each of the filters into a function (so that it can be
// instantiated with different input streams).



// Filter f1: Project only price and symbol:
fun f1(input) 
  SELECT(  fun(x) x.(| PRICE, SYM ) , 
           fun(x) true,
           input )

// Filter f2: select only IBM:
fun f2(input)
  SELECT(  fun(x) x , 
           fun(x) x.SYM == "IBM",
           input )

// Filter f3: select price > 60
fun f3(input)
  SELECT(  fun(x) x , 
           fun(x) x.PRICE > 60,
           input )

// A simple helper function:
fun trace(str, S) 
  iterate x in S {
    println(str ++ x);
    emit x;
  }

// Version 1: simply instantiate them all within one "WSBlock" (process)
//============================================================
main1 = f3( f2( f1(fakestocks)))


// Version 2: insert sockets between each pair of filters.
//============================================================
/* 
  Run all within the same process, requires "-threads"
  Compile like so:
 	CC=gcc wsc2 example2.ws -threads -main main2 -o example2_main2
*/

port = 9730
s1   = trace("p1 passing on: ", f1(fakestocks))
out1 = socket_out(s1, port)

in2 :: Stream (| PRICE:Float, SYM:String)
     = socket_in("localhost", port)
s2   = trace("p2 passing on: ", f2(in2))
out2 = socket_out(s2, port+1)

in3 :: Stream (| PRICE:Float, SYM:String)
     = socket_in("localhost", port+1)
final = f3(in3)

// Note, out1/out2 have no actual contents, we are just `pulling` them to make stuff happen:
main2 = unionList([out1,out2, final])



// Version3 : separate entrypoints for each process:
//============================================================
/*
Compile each of these separately, like this:

	wsc2 example2.ws -main p1 -o p1
	wsc2 example2.ws -main p2 -o p2
	wsc2 example2.ws -main p3 -o p3 
 */

p1 = out1 
p2 = out2 
p3 = iterate x in final {
       println("  p3 received: "++x);
       emit ();
     }

