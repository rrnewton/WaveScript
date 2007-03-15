
type MyType = List Int;

s0 = audioFile("./countup.raw", 4096, 0, 44000);

BASE <- iterate(_ in s0) {
  emit ([1] :: MyType);
}
