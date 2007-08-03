

// Simple test of unionList.

s1 = (readFile("./countup.txt", "mode: binary", timer(44000.0)) :: Stream (Int16 * Int16));

s2 = iterate((i,f) in s1) { emit int16ToFloat(i) };
//s3 = iterate((i,f) in s1) { emit floatToInt(f) + 100 };
s3 = iterate((i,f) in s1) { emit int16ToFloat(f) + 100.0 };

//BASE <- s3;
//BASE <- s2;

BASE <- unionList([s2, s3]);
