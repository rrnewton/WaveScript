
// A library of matrix routines implemented directly in WaveScript.

// NOTE! matrices are indixed (row,column)
// Remember to iterate over the matrix by incrementing row on the
// outside and column in the inner loop.

// Author:  Lewis Girod & Ryan Newton 

include "stdlib.ws";


type Matrix t = (int * int * Array t);

//DEBUGMATRIX = true

// [2007.03.19] rrn: I'm going to clean things up gradually and move them into this namespace:
namespace Matrix {

// Interface:
create :: (Int, Int, t) -> Matrix t;
get    :: (Matrix t, Int, Int) -> t;
set    :: (Matrix t, Int, Int, t) -> ();
dims   ::  Matrix t               -> (Int * Int);
//copy   ::  Matrix t -> Matrix t;

 null = (0,0,Array:null);

 // For the native WS implementation, comparison is just builtin equality.
 eq = (==);

 fun create(rows, cols, init) {
   // This should be nested build OR build/make
   (rows,cols,Array:make(rows*cols, init))
 }

 fun get(mat, row, col) {
   let (colsize,rowsize,arr) = mat;
   arr[col+rowsize*row]
 }

 fun set(mat, row, col, val) {
   let (colsize,rowsize,arr) = mat;
   arr[col+rowsize*row] := val;
   () // mutators should return nothing!
 }

 fun dims(mat) {
   let (rows,cols,arr) = mat;
   (rows,cols)
 }
 
 // The matrix IS just a one dimensional array.  We don't guarantee
 // fresh storage here, so we just return it.
 fun toArray((r,c,arr)) arr 
 fun fromArray(arr, r) {
   using Array;
   c = arr`length / r;
   assert("fromArray: rows divide array length evenly", arr`length == r*c);
   (r,c,arr)
 }

 // In general build is efficient because it doesn't need to zero the storage.
 fun build(r,c,f) {
   using Array;
   arr = Array:makeUNSAFE(r*c);
   for i = 0 to r - 1 {
   for j = 0 to c - 1 {
       arr[i*c + j] := f(i,j);
     }
   };
   (r,c,arr)
 }

 // If the array is not "rectangular" this could do an out-of-bounds access.
 // Could check explicitely...
 fun fromArray2d(arr) {
   r = arr`Array:length;
   c = arr[0]`Array:length;
   // TODO: Should use Array:blit...
   Matrix:build(r,c,fun (i,j) (arr[i])[j])
 }

 // rrn: Pure version:
 // Inefficient... but generally runs only at meta-time.
 fun fromList2d(list) {
   r = list`List:length;
   c = list`head`List:length;
   Matrix:build(r,c,fun (i,j) 
       List:ref(List:ref(list,i), j) )
 }

 // Note, these provide no guarantees as to allocating fresh storage:
 fun row(m,i) {
   let (r,c) = dims(m);
   Array:build(r,fun(j) Matrix:get(m,i,j))
 }
 fun col(m,j) {
   let (r,c) = dims(m);
   Array:build(c,fun(i) Matrix:get(m,i,j))
 }

/*  maybe faster versions... but hopefully optimizer will beat this
 fun row(m,i) {
   let (rowsize,a) = m;
   Array:build(rowsize,fun(j) a[i*rowsize+j])
 }
 fun col(m,j) {
   let (rowsize,a) = m;
   colsize = a`Array:length / rowsize;
   Array:build(colsize,fun(i) a[i*rowsize+j])
 }
*/

 fun foreachi(f, mat) {
   let (r,c) = dims(mat);
   for j = 0 to c-1 {
     for i = 0 to r-1 {
       f(i,j, get(mat,i,j))
     }
   }
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


