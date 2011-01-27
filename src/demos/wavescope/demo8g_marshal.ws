
// This demonstrates the marshal and unmarshal functions.

fun marsh_prnt(x) {
  print("ORIG            : "++x++"\n");
  m1 = marshal(x);
  print("marshaled len "++Array:length(m1)++": "++m1++"\n");
  y = unmarshal(m1,0);
  print("    unmarshaled : "++y++"\n\n");
  z = [x,y]; // Hack, force them to be the same type.
  y
}

main = iterate _ in timer(10) {

  print$ "An integer:\n";
  marsh_prnt$ 33;

  print$ "An int16:\n";
  marsh_prnt$ (33::Int16);

  print$ "An array:\n";
  a = marsh_prnt$ Array:build(4, fun(i) i);
  
  print$ "A list:\n";
  l = marsh_prnt$ List:build(4, fun(i) Uint8! i);

  print$ "A tuple with a string:\n";
  marsh_prnt$ (3, 4, "five");

  print$ "Tuple containing array and list:\n";
  marsh_prnt$ (l, a);
  
  // More nesting 
  //y = ([#[1],#[2]], #[[3,4], []]);

  emit ();
}
