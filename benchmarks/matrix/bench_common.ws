
matsize = 500;

fun runtest(f) {
  m1 = Matrix:build(matsize,matsize,fun (i,j) intToDouble(i+j));
  m2 = Matrix:build(matsize,matsize,fun (i,j) intToDouble(i*j));
  
  start1 = clock();
  
  m3 = f(m1,m2);
  
  start2 = clock();
  
  println("STARTTIMECPU: "++start1);
  println("ENDTIMECPU: "++start2);
  
  sum = Mutable:ref(gint(0));
  for i = 0 to matsize-1 {
      for j = 0 to matsize-1 {
	sum := sum + Matrix:get(m3,i,j);
      }
  };
  
  println("sum="++sum);
  
};

