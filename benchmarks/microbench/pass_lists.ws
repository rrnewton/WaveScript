
// Communication through allocating and passing lists:

bufsize = if GETENV("BUFSIZE") == "" then 100 else stringToInt(GETENV("BUFSIZE"));

s1 = iterate _ in timer(1000) {
  emit List:build(bufsize, fun(i) i)
}

main = iterate ls in s1 { emit List:fold((+),0, ls) }
