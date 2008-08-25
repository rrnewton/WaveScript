
include "stdlib.ws"

//====================================================================================================
/// General helpers:

/// Nested arrays
// Note, these functions should be replaced with a proper multi-dimensional matrix library.

//====================================================================================================
// 3D arrays, nested vs. implementation:

// We can toggle here which 3D array library to use:
//include "array3d_nested.ws"
include "array3d_flat.ws"

//====================================================================================================

fun Array:make4D(i,j,k,l, val) {
  using Array;
  build(i, fun(_) 
  build(j, fun(_)
  build(k, fun(_)
   make(l, val))));
}


//====================================================================================================
// Test code:

main = iterate _ in timer(3) {
  using Array3D; using Mutable;
  println("Testing Array3D...");
  assert_eq("length of null Array3D", (0,0,0), null.dims);

  let (r,c,h) = (10,8,13);
  
  a = build(r,c,h, fun(i,j,k) i+j+k);
  b = make(r,c,h, 9999);

  //println("Matrix: "++b++"\n");
  //println("Dims: "++b.dims);

  assert_eq("dims equal", a.dims, b.dims);
  assert_eq("dims right", a.dims, (r,c,h));

  map_inplace2(b,a, fun(y,x) x);

  assert_eq("map2 inplace used to copy array", a, b);

  fun sum(m) fold2(m,m, 0, fun(acc,x,_) acc+x);

  m = make(r,c,h, 1);

  assert_eq("fold for summing elements", r*c*h, sum(m));

  //println$ "\n\n"++m++"\n";

  /*for i=0 to r-1 {
  for j=0 to c-1 {
  for k=0 to h-1 {
    print(get(m, i,j,k) ++ " ");
  }}};*/

  //println("\nNow setting:\n");
  cnt = ref$ 0;
  for i=0 to r-1 {
  for j=0 to c-1 {
  for k=0 to h-1 {
      //print(get(m, i,j,k) ++ " ");
    set(m, i,j,k, 0);
    cnt += 1;
  }}};

  //println$ "\n\n"++m;
  assert_eq("Completely zeroed", 0, sum(m));  

  for i=0 to r-1 {
  for j=0 to c-1 {
  for k=0 to h-1 {
    assert_eq("definetly zero", 0, get(m, i,j,k));
  }}};

  map_inplace(m, fun(x) x+1);
  assert_eq("sums correctly", r*c*h, sum(m));
  
  println("  passed all tests");
  emit true;
}
