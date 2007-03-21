

// Read a text file.
//BASE <- (dataFile("./countup.txt", "text", 44000, 0) :: Stream (Int * Float))

BASE <- (dataFile("./countup.raw", "binary", 44000, 0) :: Stream (Int ))
