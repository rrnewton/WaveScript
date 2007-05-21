
type MyType = List Int;
type MyType2 t = Stream (List t);
type MyType3 (x) = List (x);
type MyType4 (x,y) = List (x * y);

s0 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));

s1 :: MyType2 Int;
s1 = iterate(_ in s0) {
  emit ([1] :: MyType);
}

val = (s1,s1);

BASE <- s1;
