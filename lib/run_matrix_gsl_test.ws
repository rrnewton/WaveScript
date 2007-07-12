include "stdlib.ws";
include "matrix_gsl.ws";

f2d = floatToDouble;

result = iterate _ in timer(30.0) 
{ 
  using Matrix; using Float;

  m = create(2,3);
  print("Ref: "++ get(m,0,0)  ++"\n");
  print("Ref: "++ get(m,1,2)  ++"\n");
  set(m, 1,2, 3.0);
  set(m, 1,1, 9.0);
  print("  Set... \n");
  print("  Ref: "++ get(m,1,2)  ++"\n");
  arr = m ` toArray;
  print(" Converted to array: "++ arr ++"\n");
  mat2 = fromArray(arr,2);
  print(" Converted back to matrix: "++ mat2 ++"\n");
  print(" And back to array : "++ mat2 ` toArray ++"\n\n");

  using Matrix; using Double;

  newmat = create(3,3);
  set(newmat, 0,0, f2d$ 1.0);
  set(newmat, 1,1, f2d$ 2.0);
  set(newmat, 2,2, f2d$ 3.0);
  
  print("A double matrix    : "++ newmat `toArray ++"\n");

  dub = copy(newmat); 

  print("A copy             : "++ dub `toArray ++"\n");
  assert("matrix copy eq", eq(newmat,dub));

  inv = Matrix:Generic:invert(dub);

  print("It's inverse       : "++ inv `toArray ++"\n");
  doubleinv = inv `Matrix:Generic:invert;
  print("It's double inverse (using generic): "++ doubleinv `toArray ++"\n");
  assert("double inverse eq", eq(dub,doubleinv));

  // Mutates it:
  add_constant(dub, f2d$ 3.3);
  print("Add a constant:: "++ dub `toArray ++"\n");  
  add_constant(dub, f2d$ 3.3);

  scratch = copy(dub);
  sub(scratch, newmat); // Just the difference
  scale(scratch, f2d$ -1.0);
  add(dub,scratch);
  print("Sub back out and get back: "++ dub `toArray ++"\n");  
  assert("add sub eq", eq(dub,newmat));

  scale(dub, f2d$ 10.0);
  print("scale         :: "++ dub `toArray ++"\n");  
  add(dub,dub);
  print("add self      :: "++ dub `toArray ++"\n");  

  //add_constant(dub, f2d$ 1.0);  
  product = mul(dub,dub);
  print("a mult      :: "++ product `toArray ++"\n");  
  
  emit m;
  emit dub;
  emit Matrix:Complex:create(3,3);
  //  emit m ` invert;
}

BASE <- result
//BASE <- Matrix:noTrans
