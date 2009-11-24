
//include "stdlib.ws";

ls1 = [1,2,3,4]
//ls2 = List:build(5, fun(i) Array:build(i,fun(j)j))
ls2 = List:build(3, fun(i) Array:build(i,fun(j)j))

main = iterate _ in timer(3.0) { emit (ls1,ls2) }

/*
iterate _ in timer(3.0) {
  emit 
}
*/
