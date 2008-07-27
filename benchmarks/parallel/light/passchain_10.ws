

main = {
  fun f(n,s) 
    if n == 0 then s else
    iterate x in f(n-1,s) { emit x };
  f(10, timer(30))
}
