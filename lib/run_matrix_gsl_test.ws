include "matrix_gsl.ws";

f2d = floatToDouble;

BASE <- iterate _ in timer(30.0) 
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

  dub = create(3,3);
  set(dub, 0,0, f2d$ 1.0);
  set(dub, 1,1, f2d$ 2.0);
  set(dub, 2,2, f2d$ 3.0);
  
  print("A double matrix    : "++ dub `toArray ++"\n");
  inv = Matrix:Generic:invert(dub);
  print("It's inverse       : "++ inv `toArray ++"\n");
  print("It's double inverse: "++ inv `Matrix:Generic:invert `toArray ++"\n");

  // Mutates it:
  add_constant(dub, f2d$ 3.3);
  print("Add a constant:: "++ dub `toArray ++"\n");  
  scale(dub, f2d$ 10.0);
  print("scale         :: "++ dub `toArray ++"\n");  
  add(dub,dub);
  print("add self      :: "++ dub `toArray ++"\n");  
  
  emit m;
  emit dub;
  emit Matrix:Complex:create(3,3);
  //  emit m ` invert;
}
