

// Simple test of unionList.

s1 = (dataFile("./countup.txt", "text", 44000, 0) :: Stream (Int * Float));

s2 = iterate((i,f) in s1) { emit i };
s3 = iterate((i,f) in s1) { emit floatToInt(f) + 100 };

//BASE <- s3;

BASE <- unionList([s2, s3]);
