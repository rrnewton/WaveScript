
// Drive WS from the outside, from "foreign_sources"

src1 = (foreign_source("wsentry1", ["source.c"]) :: Stream Int)

// Here's a hackish feature, a single outer tuple is specially handled
// by calling a multi-argument function to invoke WS.
//src2 = (foreign_source("wsentry2", ["source.c"]) :: Stream (Int * Float))
src2 = (foreign_source("wsentry2", ["source.c"]) :: Stream (Float))

s1 = iterate i in src1 { 
  print("                            Got input on src1: "++i++"\n");
  emit ();
}

s2 = iterate x in src2 {
  print("                            Got input on src2: "++x++"\n");
  emit ();
}

main = iterate x in unionList([s1,s2]) {
  //print("Got from foreign: "++x++" \n");
  emit x;
}
