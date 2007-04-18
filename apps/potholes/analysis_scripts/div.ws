












BASE <- 
iterate (x,y) in (readFile("/dev/stdin","") :: Stream (Int * Int)) {
  print(x ++" "++ intToFloat(y) / 258021. ++"\n");
  emit ()
}
