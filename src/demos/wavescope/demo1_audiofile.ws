

// Read an audio file.
s1 :: Stream (Sigseg Int);
s1 = audioFile("./countup.raw", 10, 0, 44000);

BASE <- s1;
