

// WaveScript Semantics merges multiple main functions implicitly.

a = iterate _ in timer(-1.0) { emit 'a' }
b = iterate _ in timer(0)    { emit 'b' }
c = iterate _ in timer(2)    { emit 'c' }
d = iterate _ in timer(3)    { emit 'd' }

main = a
// main = a.merge(b).merge(c).merge(d)
