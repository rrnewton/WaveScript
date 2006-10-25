
fun const_strm(x) {
  iterate (_ in timer(3000)) {
    emit x;
  }
}


// Update an entry in an association list.
// Not tail-recursive.
fun alist_update(ls, ind, new) {
  let (x,y) = ls.head;
  if x == ind
  then (x,new) :: ls.tail
  else (x,y) :: alist_update(ls.tail, ind, new)
}

fun alist_lookup(ind, ls) {
  let (x,y) = ls.head;
  if x == ind
  then (x,y)
  else alist_lookup(ind, ls.tail)
}

fun fold(f, zero, ls) {
  print("Folding..."++ show(f) ++" "++ show(zero) ++" "++ show(ls)++ "\n");
  fun loop(acc, ls) {
    if ls == []
    then acc
    else loop( f(acc, ls.head), ls.tail)
  };
  loop(zero,ls)
}


fun test_lists() {
  print("Testing list module.\n");
  print([1,2,3]);
  //  print(fold((+), 0, [1,2,3]));
  
  //  98
}


BASE <- const_strm(test_lists());

