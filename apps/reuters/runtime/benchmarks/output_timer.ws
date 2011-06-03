
include "stdlib.ws"

// This is a user defined function that corresponds to an operator used by WSQ:
fun output_timer( instrm, arg1 )
{
  print("Creating a measurement operator to print the number of tuples received at intervals of "++arg1++" Hz.\n");
  freq = stringToFloat(arg1);

  // This produces an empty output stream:
  iterate x in union2(instrm, timer(freq)) {
    state { c = 0; 
            total = 0;
	    intervals = -1; // The first one doesn't count.
          }

    case x {
      Left(x): {
	c += 1;
      }
      Right(_): {
	total     += c;
	intervals += 1;
	if (intervals > 0) then
  	  print("Counted "++c++" tuples in interval "++intervals++" average "++ (total / intervals) ++ "\n");
        c := 0;
      }
    }
  };
}
