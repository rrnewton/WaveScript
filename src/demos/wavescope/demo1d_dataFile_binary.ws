// Now read a binary file with 'dataFile'.

//s1 = (dataFile("./countup.raw", "binary", 44000, 0) :: Stream (Int16 * Int16 * Int16));
s1 = (dataFile("./countup.raw", "binary", 44000, 0) :: Stream Int16);

//s1 = (dataFile("./6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream Int16);
//s1 = (readFile("./6sec_marmot_sample.raw", "mode: binary") :: Stream Int16);
//s1 = (readFile("./6sec_marmot_sample.raw", "mode: binary  rate: 24000  window: 4096  skipbytes: 6 ") :: Stream (Sigseg Int16));

BASE <- s1
//BASE <- iterate((x,_,_) in s1) { emit x + gint(1000) }
