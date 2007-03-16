
s0 = audioFile("./countup.raw", 4096, 0, 44000);

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

BASE <- iterate(w in s0) {  
  n = w\width;
  i = n\intToInt16;
  f = n\intToFloat;
  c = n\intToComplex;

  print("\nFrom int16:\n");
  println( i\int16ToInt );
  println( i\int16ToFloat );
  println( i\int16ToComplex );

  print("\nFrom int:\n");
  println( i );
  println( f );
  println( c );

  print("\nFrom float:\n");
  println( f\floatToInt16 );
  println( f\floatToInt );
  println( f\floatToComplex );

  print("\nFrom complex:\n");
  println( c\complexToInt );
  println( c\complexToInt16 );
  println( c\complexToFloat );

  print("\nFrom string:\n");
  //println( i \ show \ stringToInt16 );
  println( n \ show \ stringToInt );
  println( f \ show \ stringToFloat );
  println( c \ show \ stringToComplex );

  print("\n");

  emit ();
}
