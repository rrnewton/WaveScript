
// The same interface as matrix-rowmajor.ws but for 3D matrices.

include "stdlib.ws";

// rather than store the r,c,h dimensions, this representation
// stores (r, c*h, h) for quicker lookups.
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
   (rows, cols*height, height, Array:make(rows*cols*height, init))

   fun get((r,ch,h, arr), i, j, k)        arr[i*ch + j*h + k]
   fun set((r,ch,h, arr), i, j, k, val)   arr[i*ch + j*h + k] := val
   /*
 fun get((r,c,ch, arr), i, j, k)        {
   println("Getting ind "++(i,j,k)++" bounds "++(r,c,ch)++" array ind "++i*ch + j*c + k);
   arr[i*ch + j*h + k]
 }
 fun set((r,c,ch, arr), i, j, k, val)   {
   println("Setting to "++val++", ind "++(i,j,k)++" bounds "++(r,c,ch)++" array ind "++i*ch + j*c + k);
   arr[i*ch + j*h + k] := val
 }
   */

 // Dims has to do a division.
 fun dims((r,ch,h, arr))
   if r == 0 then (0,0,0) else (r,ch/h,h)

 // In general build is efficient because it doesn't need to zero the storage.
 fun build(r,c,h, fn) {
   using Array;
   ch = c*h;
   arr = Array:makeUNSAFE(r*ch);
   for i = 0 to r-1 {
   for j = 0 to c-1 {
   for k = 0 to h-1 {
       arr[i*ch + j*h + k] := fn(i,j,k);
   }}};
   (r,ch,h,arr)
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

 fun fold((_,_,_, arr), zer, fn) {
  acc = ref$ zer;
  Array:foreach(fun(x) acc := fn(acc, x), arr);
  acc
 }

 null = (0,0,0,Array:null);

} // End namespace
