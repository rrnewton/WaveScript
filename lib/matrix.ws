
// A library of matrix routines implemented directly in WaveScript.

// NOTE! matrices are indixed (row,column)
// Remember to iterate over the matrix by incrementing row on the
// outside and column in the inner loop.

// Author:  Lewis Girod & Ryan Newton 

include "stdlib.ws";


type Matrix t = Array (Array t);

//DEBUGMATRIX = true

// [2007.03.19] rrn: I'm going to clean things up gradually and move them into this namespace:
namespace Matrix {


// Interface:
create :: (Int, Int, t) -> Matrix t;
get    :: (Matrix t, Int, Int) -> t;
set    :: (Matrix t, Int, Int, t) -> ();
dims   ::  Matrix t               -> (Int * Int);
//copy   ::  Matrix t -> Matrix t;

 // For the native WS implementation, comparison is just builtin equality.
 eq = (==);

 fun create(rows, cols, init) {
   // This should be nested build OR build/make
   arr = Array:make(rows, Array:null);
   for i = 0 to rows-1 {
     arr[i] := Array:make(cols, init);
   };
   arr
 }

 fun get(mat, row, col) (mat[row])[col];
 fun set(mat, row, col, val) {
   r = mat[row];
   r[col] := val;
   () // mutators should return nothing!
 }

 fun dims(mat) (Array:length(mat), Array:length(mat[0]))

 // Here we pack the Array of Arrays into a one-dimensional array for
 // consistency with the GSl interface.
 fun toArray(mat) {
   // Could use Array:build but, we'd have to do division.
   let (r,c) = Matrix:dims(mat);
   arr = Array:makeUNSAFE(r*c);
   for i = 0 to r - 1 {
     for j = 0 to c - 1 {
       Array:set(arr, i + (j*r), Matrix:get(mat,i,j));
     }
   };
   arr
 }
 fun fromArray(arr, r) {
   using Array;
   c = arr`length / r;
   assert("fromArray: rows divide array length evenly", arr`length == r*c);
   build(r, fun(i)
      build(c, fun(j) 
        arr[i + (j*r)]))
 }
 // No guarantee to copy storage!!
 fun fromArray2d(arr) arr

 // In general build is efficient because it doesn't need to zero the storage.
 fun build(r,c,f) {
   using Array;
   build(r, fun(i) build(c, fun(j) f(i,j)))
 }

 // rrn: Pure version:
 // Inefficient... but generally runs only at meta-time.
 fun fromList2d(list) {
   len2 = list`head`List:length;
   Array:build(list`List:length,
     fun(i) Array:build(len2,
       fun(j) List:ref(List:ref(list,i), j)))
 }



 // Note, these provide no guarantees as to allocating fresh storage:
 fun row(m,i) m[i]
 fun col(m,j) {
   let (r,c) = Matrix:dims(m);
   arr = Array:makeUNSAFE(r);
   for i = 0 to r-1 {
     arr[i] := Matrix:get(m,i,j);
   };
   arr
 }

 fun foreachi(f, mat) {
   let (r,c) = dims(mat);
   for i = 0 to r-1 {
     for j = 0 to c-1 {
       f(i,j, get(mat,i,j))
     }
   }
 }

 fun fold(fn, zer, mat) {
   acc = Mutable:ref(zer);
   foreachi(fun(i,j,x) acc := fn(acc,x), mat);
   acc
 }

 fun map_inplace(f, mat) {
   foreachi(fun(i,j,x) set(mat, i,j, f(x)), mat)
 }
/*
 {
   using Matrix;
   let (r,c) = dims(mat);
   for i = 0 to r-1 {
     for j = 0 to c-1 {
       set(mat, i,j, f(get(mat,i,j)))
     }
   }
 }
*/

 fun map2_inplace(f, mat1, mat2) {
   foreachi(fun(i,j,x) set(mat1, i,j, f(x, get(mat2,i,j))),  mat1)
 }

   /*
 {
   using Matrix;
   let (r,c) = dims(mat1);
   for i = 0 to r-1 {
     for j = 0 to c-1 {
       set(mat1, i,j, f(get(mat1,i,j), get(mat2,i,j)))
     }
   }
 }
   */
}

include "matrix-common.ws";

