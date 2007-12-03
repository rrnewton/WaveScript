
s0 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

fun assert_eq(a,b) if not(a==b) then wserror("Assert failed: "++ a ++" not equal "++ b);

main = iterate(w in s0) {  
  n = w`width;
  i = n`intToInt16;
  l = n`intToInt64;
  f = n`intToFloat;
  d = n`intToDouble;
  c = n`intToComplex;

  // Roundtripping: 
  // 4096 is representable as any of our numbers and as such should be convertible without loss:
  assert_eq(n, i`int16ToInt);
  assert_eq(n, l`int64ToInt);
  assert_eq(n, f`floatToInt);
  assert_eq(n, d`doubleToInt);
  assert_eq(n, c`complexToInt);

  print("\nFrom int16:\n");
  println( i`int16ToInt );
  println( i`int16ToInt64 );
  println( i`int16ToFloat );
  println( i`int16ToDouble );
  println( i`int16ToComplex );

  print("\nFrom int:\n");
  println( i );
  println( l );
  println( f );
  println( d );
  println( c );

  print("\nFrom float:\n");
  println( f`floatToInt16 );
  println( f`floatToInt64 );
  println( f`floatToInt );
  println( f`floatToDouble );
  println( f`floatToComplex );

  print("\nFrom complex:\n");
  println( c`complexToInt );
  println( c`complexToInt16 );
  println( c`complexToInt64 );
  println( c`complexToDouble );
  println( c`complexToFloat );

  print("\nFrom string:\n");
  //println( i ` show ` stringToInt16 );
  println( n ` show ` stringToInt );
  println( f ` show ` stringToFloat );
  println( f ` show ` stringToDouble );

  // [2007.06.30] Leaving this one off for now:
  //  println( c ` show ` stringToComplex );

  print("\n");


  emit ();
}
