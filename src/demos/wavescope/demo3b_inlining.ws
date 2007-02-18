
// Simple function should inline.
fun marmotscore(w) { 3.8 }

//ch1 = audio(0, 4096, 0, 44000);
ch1 = audioFile("./countup.raw", 4096, 0, 44000);

newwidth = 32;
step = 32;
rw1 = iterate (w in ch1) {
  emit( marmotscore(w) );
};

BASE <- rw1;
