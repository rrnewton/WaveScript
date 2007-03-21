

// Construct a chain of operators 
fun loop (n, S) {
  if n == 0 then S 
  else loop(n-1, iterate((time,x) in S) { emit (time,x+1) })
}

BASE <- loop(10, (iterate (() in timer(30.0)) {emit (99,0)}))
