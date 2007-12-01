
























include "stdlib.ws";

// This uses the new convention that a top-level stream named "main" is the returned stream of the program.

main = (readFile("./countup.txt", "mode: text", timer(1000.0))
        :: Stream (Int16 * Float));

// nullsink <- main;
// nullsink(main);

/*

fun add1(n) { n + 1 }

fun b1(strm) {
  foo :: Stream Float = 
    iterate (i,f) in strm {
      emit f * 100;
      emit add1(f);
    };
  bar = iterate x in foo { emit x - 1 };
  bar
}

fun b2(strm) {
  iterate f in strm {
    println("  "++show(f+1.0));
    //if false then emit 3.0;
  }  
}


//BASE(b1(s1));
//MAIN = b1(s1);

BASE <- b2(b1(s1));

     */
