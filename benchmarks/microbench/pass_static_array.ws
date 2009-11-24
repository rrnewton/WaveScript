
// Communication through a static communication buffer (array)

bufsize = if GETENV("BUFSIZE") == "" then 100 else stringToInt(GETENV("BUFSIZE"));

s1 = iterate _ in timer(1000) {
  state { commbuf = Array:makeUNSAFE(bufsize) }
  for i = 0 to bufsize-1 {
    commbuf[i] := i;
  }
  emit commbuf;
}

main = iterate x in s1 { emit Array:fold((+),0, x) }
