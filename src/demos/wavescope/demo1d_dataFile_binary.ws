// Now read a binary file with 'dataFile'.

s1 = (dataFile("./countup.raw", "binary", 44000, 0) :: Stream (Int16 * Int16 * Int16));

//BASE <- s1
BASE <- iterate((x,_,_) in s1) { emit x + gint(1000) }
