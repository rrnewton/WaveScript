

/* 
 One sticky point about the multi-stage evaluation is that there have
 been problems keeping the types straight when evaluating a value at
 compile time. (In particular because of the generic arithmetic.)
*/

main = iterate _ in timer$ 10 {
  state { 
    xx = (Array:make(3, (2::Int16)), (10::Int64), ([] :: List Float));
  }
  let (arr, n, ls) = xx;
  n2 :: Int64 = n + 1;
  emit (arr, n2, ls);
}
