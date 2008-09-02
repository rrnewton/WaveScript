
// A pipeline of kernels that each passover an input buffer many times.

fun trylookup(str,def) {
  if GETENV(str) == "" 
  then def
  else stringToInt(GETENV(str));
}

bufsize = trylookup("BUFSIZE", 1000)
reps    = trylookup("REPS", 4000)
workers = trylookup("WORKERS", 12)
rate    = Float! trylookup("TIMERRATE", 1000)

_ = println("Total ops per tick: "++ bufsize * reps)

src = iterate _ in timer(rate) {
  emit build(bufsize, fun(i) Int64! i)
}

fun work(x) {
  sum = 0; // Ignore overflow -- note, mlton
  //println("Working it...\n");
  for i = 1 to reps {
    sum += Int64! fold((+),0, x)	
  };
  make(length(x), sum);
}

main = {
  fun f(n,s) 
    if n == 0 then s else
    iterate x in f(n-1,s) { 
      emit work(x);
    };
  iterate _ in f(workers, src) { emit () }
  //f(workers, src)
}
