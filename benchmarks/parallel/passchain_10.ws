
// This creates some heavyweight worker boxes that do a million sqrts.

// On my nokia Core2 duo 3ghz:
// 1000 tuples with ops = 1 million takes 61.3s real on 2 threads
// In single threaded mode it takes 100.6 seconds

// That is not quite the perfect speedup that one would expect on something ilke this.

ops = 1000 * 1000
threads = 10
rate = 30

fun work(fl) {
  x = Mutable:ref(fl);
  for i = 0 to ops {
    x := sqrtF(x);
    x := x*x;
  }; 
  x
}

main = {
  fun f(n,s) 
    if n == 0 then s else
    iterate x in f(n-1,s) { emit work(x) };
  f(threads, iterate _ in timer(rate) { emit 99 } )
}
