
type MyType = List Int;
type MyType2 t = Stream (List t);
type MyType3 (x) = List (x);
type MyType4 (x,y) = List (x * y);

s0 = audioFile("./countup.raw", 4096, 0, 44000);

BASE <- iterate(_ in s0) {
  emit ([1] :: MyType);
}
