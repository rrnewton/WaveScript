
// The same interface as matrix-rowmajor.ws but for 3D matrices.

include "stdlib.ws";

// rather than store the i,j,k dimensions, this representation
// stores (i, j, j*k) for quicker lookups.
type Matrix3D t = (int * int * int * Array t);

//DEBUGMATRIX = true

// [2007.03.19] rrn: I'm going to clean things up gradually and move them into this namespace:
namespace Matrix3D {

// Interface:
create :: (Int, Int, Int, t) -> Matrix3D t;
get    :: (Matrix3D t, Int, Int, Int) -> t;
set    :: (Matrix3D t, Int, Int, Int, t) -> ();
dims   ::  Matrix3D t                    -> (Int * Int * Int);

 // For the native WS implementation, comparison is just builtin equality.
 eq = (==);

 fun create(rows, cols, height, init) 
   // This should be nested build OR build/make
   (rows, cols, cols*height, Array:make(rows*cols*height, init))

 fun get((r,c,ch, arr), i, j, k)        arr[i*ch + j*c + k]
 fun set((r,c,ch, arr), i, j, k, val)   arr[i*ch + j*c + k] := val

 // Dims has to do a division.
 fun dims((r,c,ch, arr))  (r,c,ch/c)

 // In general build is efficient because it doesn't need to zero the storage.
 fun build(r,c,h, fn) {
   using Array;
   ch = c*h;
   arr = Array:makeUNSAFE(r*ch);
   for i = 0 to r-1 {
   for j = 0 to c-1 {
   for k = 0 to h-1 {
       arr[i*ch + j*c + k] := fn(i,j,k);
   }}};
   (r,c,ch,arr)
 }

 fun fill((_,_,_,arr), x) {
   Array:fill(arr,x);
   ()
 }

 fun foreach((_,_,_,arr),fn)                 Array:foreach(fn,arr)
 fun foreach2((_,_,_,arr1),(_,_,_,arr2),fn)  Array:foreach2(fn,arr1,arr2)

 fun map_inplace(mat, fn) {
   using Array;
   let (_,_,_,a) = mat;
   for i = 0 to a.length-1 {
     a[i] := fn(a[i]);
   };
   mat
 }
 fun map2_inplace(matA,(_,_,_,b), fn) {
   using Array;
   let (_,_,_,a) = matA;
   for i = 0 to a.length-1 {
     a[i] := fn(a[i], b[i]);
   };
   matA
 }

 // Could always look for fine-grained parallelism here in the future.
 fun fold2((_,_,_,a),(_,_,_,b), zer, fn) {
   acc = Mutable:ref(zer);
   for i = 0 to Array:length(a)-1 {
     acc := fn(acc, a[i], b[i]);
   };
   acc
 }

} // End namespace

