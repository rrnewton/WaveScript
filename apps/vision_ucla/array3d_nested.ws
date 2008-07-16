
type Array3D t = Array (Array (Array t)); 

Array3D:null = Array:null

fun Array3D:fill(arr, val) {
  for i = 0 to Array:length(arr)-1 {
   for j = 0 to Array:length(arr[i])-1 {
       Array:fill(arr[i][j], val);
     }};
}


fun Array3D:make(i,j,k, val) {
  using Array;
  build(i, fun(_)
  build(j, fun(_)
   make(k, val)));
}
fun Array3D:build(r,c,h, fn) {
  using Array;
  build(r, fun(i)
  build(c, fun(j)
  build(h, fun(k) fn(i,j,k))));
} 

// Assumes they're of the same dimension.  If not, you'll get an
// unhelpful out of bounds error.
fun Array3D:foreach2(arr1, arr2, fn) {
  using Array;
  //if DEBUG then assert_eq("foreach2_3D: length mismatch", arr1.length, arr2.length);
  for i = 0 to length(arr1)-1 {
   //if DEBUG then assert_eq("foreach2_3D: inner length mismatch", arr1[i].length, arr2[i].length);
   for j = 0 to length(arr1[i])-1 { 
       //if DEBUG then assert_eq("foreach2_3D: inner inner length mismatch", arr1[i][j].length, arr2[i][j].length);
    for k = 0 to length(arr1[i][j])-1 {
      fn(arr1[i][j][k], arr2[i][j][k])
  }}};
}

fun Array3D:foreach(arr, fn) {
  using Array;
  for i = 0 to length(arr)-1 {
   for j = 0 to length(arr[i])-1 { 
    for k = 0 to length(arr[i][j])-1 {
      fn(arr[i][j][k])
  }}};
}

fun Array3D:fold(arr, zer, fn) {
  acc = ref$ zer;
  Array3D:foreach(arr, fun(x) acc := fn(acc, x));
  acc
}

// A fold over two 3D arrays:
fun Array3D:fold2(arr1, arr2, zer, fn) {
  acc = ref$ zer;
  Array3D:foreach2(arr1, arr2, fun(a,b) acc := fn(acc, a, b));	 
  acc
}

fun Array3D:map_inplace(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
     arr[i][j][k] := fn(arr[i][j][k]);
  }}};
  arr
}

fun Array3D:map_inplace2(arr, arr2, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
    arr[i][j][k] := fn(arr[i][j][k], arr2[i][j][k]);
  }}}
}

fun Array3D:get(arr,i,j,k)   arr[i][j][k]
fun Array3D:set(arr,i,j,k,x) arr[i][j][k] := x


fun Array3D:iter(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
    fn(i,j,k)
  }}}  
}

fun Array3D:dims(arr) {
  using Array;
  if arr == null
  then (0,0,0)
  else (arr.length, arr[0].length, arr[0][0].length);
}
