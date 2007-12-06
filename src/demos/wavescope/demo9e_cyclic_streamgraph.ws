
// Some standard type definitions.

uniontype Union2 a b       = Left a | Right b;

fun stream_map(f,s)
  iterate x in s {
    emit f(x);
  }

fun union2(s1,s2) 
  merge(stream_map(Left,s1),
	stream_map(Right,s2))

fun route2(s) 
 (iterate x in s { case x { Left(x): emit x   Right(x): {}     } },
  iterate x in s { case x { Left(x): {}       Right(x): emit x } })


fun takeLeft (s) iterate x in s { case x { Left(x): emit x   Right(x): {}     } }
fun takeRight(s) iterate x in s { case x { Left(x): {}       Right(x): emit x } }

/*
feedbackloop :: (Stream t, Stream t -> Stream t) -> Stream t;
fun feedbackloop(s0, f) {
  s1 = f(merge(s0,s1));
  s1
}
*/

s1 = iterate _ in timer(3.0) {
  state { n = 0 }
  n += 1;
  emit n;
}

/* s2 =  */
/*  iterate n in merge(s1, takeLeft(s2)) { */
/*    if n < 5  */
/*    then emit Right(n)   */
/*    else emit Left(n-1) // Feedback */
/* } */


fun feedbackUnion2(s0, f) 
     takeRight( feedbackloop(stream_map(Left,s0), f))

s2 = takeRight$
 feedbackloop(stream_map(Left,s1), 
  fun(loopback) 
   iterate x in loopback {
     case x {
       Left(n):
        if n < 5 
        then emit Right(n)  
        else emit Left(n-1) // Feedback     
       Right(n): {}
     }
   })

//let (foo,bar) = route2(s2);

//s2a = iterate x in s2 { case x { Left(x): emit x   Right(x): {}     } };
//s2b = iterate x in s2 { case x { Left(x): {}       Right(x): emit x } };

main = s2

/* 
Could use this syntactic sugar:

let (s3,s4) = route2$
  iterate2 x in s1, s2 {
   state{ }
      emit Left(x)
   OR 
      emit Right(x)
 }
*/

