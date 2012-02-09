
include "unix.ws"


// This is a user defined function that corresponds to an operator used by WSQ:

// fun myUDF( instrm, arg1, arg2)
fun myUDF( instrm, n1, extractor_fun)
{

  println("WS COMPILETIME: Inside UDF, called with args " ++ (n1,extractor_fun));

  // Here we parse the arguments at METAPROGRAM EVAL time.   
  //  n1 = stringToInt(arg1);
  //  n2 = stringToInt(arg2);
  //  n2 = 399;
  //  println("myUDF: Parsed args: "++ (n1,n2));

  // Here we construct the output stream.  The stuff inside the iterate happens at runtime:
  outstrm = iterate x in instrm {
    state { c = 0; 
            total = 0;
          }

    println("HERE is the desired field: " ++ extractor_fun(x));

    /* if ("PRICE" == arg2) then  */
    /*      println("HERE is the price: " ++ x.PRICE) */
    /* else println("HERE is the time: " ++ x.TIME); */

    //    println("RUNTIME: Inside UDF, called with args " ++ (arg1,arg2));

    // In this simple example we will just print a message every 1000 tuples:
    c += 1;
    if (c == 1000) then { 
      c := 0;
      total += 1000;
      print("myUDF counted "++ total ++" tuples.\n");
      // In this case we do not print too often so we might as well flush:
      Unix:fflush( Unix:get_stdout() ); 

      // Let through every thousandth tuple:
      emit x;
    };
  };

  // The return value of the function is just the output stream:
  outstrm
}
