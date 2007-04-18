












BASE <- 
iterate (repeats, num) in (readFile("/dev/stdin","") :: Stream (Int * Int)) {
  state { sum = 0 }
  sum += num;
  print(repeats ++" "++ sum ++"\n");
  emit ()
}
