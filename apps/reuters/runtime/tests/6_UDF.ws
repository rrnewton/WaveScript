



// This is a user defined function that corresponds to an operator used by WSQ:

fun myUDF( instrm, arg1, arg2)
{

  // Here we parse the arguments at METAPROGRAM EVAL time.   
  n1 = stringToInt(arg1);
  n2 = stringToInt(arg2);
  println("myUDF: Parsed args: "++ (n1,n2));

  // Here we construct the output stream.  The stuff inside the iterate happens at runtime:
  outstrm = iterate x in instrm {
    state { c = 0; 
            total = 0;
          }
    // In this simple example we will just print a message every 1000 tuples:
    c += 1;
    if (c == 1000) then { 
      c := 0;
      total += 1000;
      print("myUDF"++ (n1,n2) ++" counted "++ total ++" tuples.\n");
      // Let through every thousandth tuple:
      emit x;
    };
  };

  // The return value of the function is just the output stream:
  outstrm
}
