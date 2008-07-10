
include "stdlib.ws"

//====================================================================================================
/// General helpers:

/// Nested arrays
// Note, these functions should be replaced with a proper multi-dimensional matrix library.

fun fill3D(arr, val) {
  for i = 0 to Array:length(arr)-1 {
   for j = 0 to Array:length(arr[i])-1 {
       Array:fill(arr[i][j], val);
     }};
}

// Assumes they're of the same dimension.  If not, you'll get an
// unhelpful out of bounds error.
fun Array:foreach2_3D(arr1, arr2, fn) {
  using Array;
   if DEBUG then assert_eq("foreach2_3D: length mismatch", arr1.length, arr2.length);
  for i = 0 to length(arr1)-1 {
    if DEBUG then assert_eq("foreach2_3D: inner length mismatch", arr1[i].length, arr2[i].length);
   for j = 0 to length(arr1[i])-1 { 
     if DEBUG then assert_eq("foreach2_3D: inner inner length mismatch", arr1[i][j].length, arr2[i][j].length);
    for k = 0 to length(arr1[i][j])-1 {
      fn(arr1[i][j][k], arr2[i][j][k])
  }}};
}

fun Array:map3D_inplace(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
     arr[i][j][k] := fn(arr[i][j][k]);
  }}}
}

fun Array:iter3D(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
    fn(i,j,k)
  }}}  
}

fun Array:make3D(i,j,k, val) {
  using Array;
  build(i, fun(_)
  build(j, fun(_)
   make(k, val)));
}

/*
fun Array:copy3D(src, dst) {
  using Array;
  build(src.length, fun(i)
  build(src[i].length, fun(j)
   copy(src[i][j])));
}
*/

fun Array:map3D_inplace2(arr, arr2, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
    arr[i][j][k] := fn(arr[i][j][k], arr2[i][j][k]);
  }}}
}

fun Array:make4D(i,j,k,l, val) {
  using Array;
  build(i, fun(_) 
  build(j, fun(_)
  build(k, fun(_)
   make(l, val))));
}

