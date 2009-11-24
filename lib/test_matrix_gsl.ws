
include "matrix_gsl.ws";
include "shared_matrix_test.ws";

f2d = floatToDouble;

result2 = iterate dub in result1
{ 
  print("\n==============================\n");
  print("Beginning GSL specific tests..\n\n");

  inv = Matrix:Generic:invert(dub);

  using Matrix; using Double;

  print("It's inverse       : "++ inv `toArray ++"\n");
  doubleinv = inv `Matrix:Generic:invert;
  print("It's double inverse (using generic): "++ doubleinv `toArray ++"\n");
  assert("double inverse eq", Matrix:Double:eq(dub,doubleinv));

  {
      using Matrix; using Double;
      print("\nAllocating two big 1000x1000 matrices.\n");
      bigmat1 = build(1000, 1000, fun(i,j) intToDouble(i + j));
      bigmat2 = build(1000, 1000, fun(i,j) intToDouble(i * j));
      print("\nFinished allocating, multiplying.\n");
      strt = clock();

      // This takes 8/9s on faith with vanilla GSL 
      //    1s with ATLAS
      //   600 ms with ATLAS / sse2
      //product1 = mul(bigmat1, bigmat2);
      product1 = mul(bigmat1, bigmat1);
      print("Multiplied it, cpu time was: "++ (clock() - strt) ++ "\n");
      print("Random element of product1: "++ get(product1, 100, 150) ++"\n");
      sum = Mutable:ref(gint(0));
      for i = 0 to 999 {
	for j = 0 to 999 {
	  sum += get(product1, i,j);
	}
      };
      print("Sum of product matrix: "++ sum ++"\n");
      sum := 0;
      for i = 0 to 999 {
	for j = 0 to 999 {
	  sum += get(bigmat1, i,j);
	}
      };
      print("Sum of input 1: "++ sum ++"\n");      
  };
  
  emit doubleinv;
}

BASE <- result2
//BASE <- Matrix:noTrans
