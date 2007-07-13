
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
  
  emit doubleinv;
}

BASE <- result2
//BASE <- Matrix:noTrans
