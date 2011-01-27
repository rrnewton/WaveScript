

fun stream_map(fn,strm) {
  iterate elm in strm {
    emit fn(elm);
  }
}

uniontype Union2 a b       = Oneof2 a | Twoof2 b;
uniontype Union3 a b c     = Oneof3 a | Twoof3 b | Threeof3 c;
uniontype Union4 a b c d   = Oneof4 a | Twoof4 b | Threeof4 c | Fourof4 d;
uniontype Union5 a b c d e = Oneof5 a | Twoof5 b | Threeof5 c | Fourof5 d | Fiveof5 e;

fun CONST(y) 
  iterate _ in timer(1000.0) {
    emit y
  }

fun union2(s1,s2) 
  merge(stream_map(Oneof2,s1),
	stream_map(Twoof2,s2))

fun union3(s1,s2,s3) 
  merge(stream_map(Oneof3,  s1),
  merge(stream_map(Twoof3,  s2),
        stream_map(Threeof3,s3)))

//s1 = union3(CONST((1::Int)), CONST(2.0), CONST(3.0+0.0i));
s1 = union3(CONST((1::Int)), CONST(2.0), CONST(3.0));

//s1b = union3(CONST(1.0),CONST("yay"),CONST(99));
s1b = union3(CONST(1.0),CONST('h'),CONST(99));

// [2007.09.15] FIXME: ACTIVATE THIS WHEN THE COMPILER CAN HANDLE IT:
s2 = union2(s1,s1b);
//s2 = union2(s1,s1);

main = iterate sum in s2 {
     
    case sum {
      Oneof2(x): print("Got left! "++x++"\n")
      Twoof2(y): print("Got right! "++y++"\n")
    };
    
    emit ()
}
