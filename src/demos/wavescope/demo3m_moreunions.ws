

fun stream_map(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}

uniontype Union2 a b       = Oneof2 a | Twoof2 b;
uniontype Union3 a b c     = Oneof3 a | Twoof3 b | Threeof3 c;
uniontype Union4 a b c d   = Oneof4 a | Twoof4 b | Threeof4 c | Fourof4 d;
uniontype Union5 a b c d e = Oneof5 a | Twoof5 b | Threeof5 c | Fourof5 d | Fiveof5 e;


fun union2(s1,s2) 
  _merge(stream_map(Oneof2,s1),
	stream_map(Twoof2,s2))

src = union2(timer(0.1), timer(3.0));

BASE <- iterate x in src {
     
    case x {
      Oneof2(x): print("Got left! \n")
      Twoof2(y): print("Got right! \n")
    };
    
    emit ()
}

