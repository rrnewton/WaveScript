include "stdlib.ws";
//include "matrix_gsl.ws";
//include "matrix.ws";

f2d = floatToDouble;

result1 = iterate _ in timer(30.0) 
{ 
  using Matrix; using Float;

  print("\n=======================================\n");
  print("Shared matrix tests..\n\n");  

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

  //  emit m;

  using Matrix; using Double;

  newmat = create(3,3);
  set(newmat, 0,0, f2d$ 1.0);
  set(newmat, 1,1, f2d$ 2.0);
  set(newmat, 2,2, f2d$ 3.0);

  print("A double matrix    : "++ newmat `toArray ++"\n");

  dub = copy(newmat); 

  print("A copy             : "++ dub `toArray ++"\n");
  assert("matrix copy eq", eq(newmat,dub));

  // Mutates it:
  add_constant_inplace(dub, f2d$ 3.3);
  print("Add a constant:: "++ dub `toArray ++"\n");  
  add_constant_inplace(dub, f2d$ 3.3);

  scratch = copy(dub);
  sub_inplace(scratch, newmat); // Just the difference
  scale_inplace(scratch, f2d$ -1.0);
  add_inplace(dub,scratch);
  print("Sub back out and get back: "++ dub `toArray ++"\n");  
  assert("add sub eq", eq(dub,newmat));


  one = add_constant(create(3,3), f2d$ 3.3);
  two = scale(sub(add_constant(newmat, f2d$ 3.3), newmat), f2d$ 1.0);
  print( "ONE "++ one ++ "\n");
  print( "TWO "++ two ++ "\n");
  // Do it purely too:
  assert("add sub eq pure", eq(one, two));
  assert("inequal diff sizes", not(eq(add_constant(create(2,3), f2d$ 3.3), two)));

  scale(dub, f2d$ 10.0);
  print("scale         :: "++ dub `toArray ++"\n");  
  add(dub,dub);
  print("add self      :: "++ dub `toArray ++"\n");  

  //add_constant(dub, f2d$ 1.0);  
  product = Matrix:Double:mul(dub,dub);
  print("a mult      :: "++ product `toArray ++"\n");  

  emit dub;
  //  emit Matrix:Complex:create(3,3);

}

//BASE <- result
//BASE <- Matrix:noTrans
