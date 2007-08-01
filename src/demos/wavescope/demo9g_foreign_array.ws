
// Drive WS from the outside, from "foreign_sources"

src = (foreign_source("wsentry1", ["array_source.c"]) :: Stream (Pointer "int*" * Int))

s1 = iterate (p,len) in src { 
  print("                        Unpacking array input: "++(p,len)++"\n");
  arr :: Array Int = ptrToArray(p,len);
  print("                        Got array...: "++arr++"\n");
  emit arr;
}

BASE <- s1;
