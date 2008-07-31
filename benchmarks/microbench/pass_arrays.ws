
// Communication through allocating and passing arrays.

bufsize = if GETENV("BUFSIZE") == "" then 100 else stringToInt(GETENV("BUFSIZE"));

s1 = iterate _ in timer(1000) {
  emit Array:build(bufsize, fun(i) i)
}

main = iterate x in s1 { emit Array:fold((+),0, x) }
