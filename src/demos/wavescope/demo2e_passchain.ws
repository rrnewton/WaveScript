

// Construct a chain of operators 
fun loop (n, S) {
  if n == 0 then S 
  else loop(n-1, iterate((t1,t2,x) in S) { emit (t1,t2,x+1) })
}

main = loop(10, (iterate (() in timer(30.0)) {emit (99,99,0)}))
