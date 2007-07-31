
// A library of matrix routines implemented directly in WaveScript.

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

 //========================================
 // These are implemented ON TOP of the matrix abstraction.
 // Could be shared with other matrix implementation.

 fun rowmap(f, m) {
  let (rows,_) = Matrix:dims(m);
  Array:build(rows, fun(i) f(Matrix:row(m,i)))
 }

 fun copy(mat) {
   let (r,c) = Matrix:dims(mat);
   Matrix:build(r,c, fun(i,j) Matrix:get(mat,i,j))
 }

 fun map(f, mat) {
   let (r,c) = Matrix:dims(mat);
   build(r,c, fun(i,j) get(mat,i,j))
 }
 fun map2(f, mat1,mat2) {
   let (r,c) = Matrix:dims(mat1);
   build(r,c, fun(i,j) f(get(mat1,i,j), get(mat2,i,j)))
 }

 //========================================

 fun add(m1,m2)          Matrix:map2((+), m1, m2)
 fun sub(m1,m2)          Matrix:map2((-), m1, m2)
 fun mul_elements(m1,m2) Matrix:map2((*), m1, m2)
 fun div_elements(m1,m2) Matrix:map2((/), m1, m2)
 fun scale(mat,coef)     Matrix:map((* coef), mat) 
 fun add_constant(mat,c) Matrix:map((+ c), mat) 

 fun add_inplace(m1,m2)          Matrix:map2_inplace((+), m1, m2)
 fun sub_inplace(m1,m2)          Matrix:map2_inplace((-), m1, m2)
 fun mul_elements_inplace(m1,m2) Matrix:map2_inplace((*), m1, m2)
 fun div_elements_inplace(m1,m2) Matrix:map2_inplace((/), m1, m2)
 fun scale_inplace(mat,coef)     Matrix:map_inplace((* coef), mat) 
 fun add_constant_inplace(mat,c) Matrix:map_inplace((+ c), mat) 

 // Matrix multiplication.
 fun mul(m1,m2) {
  using Array;
  // TODO: could be more defensive here, check for null:
  m3 = Matrix:create(m1`length, m2[0]`length, Matrix:get(m1,0,0));
  for i = 0 to m1`length-1 {
    for j = 0 to m2[0]`length-1 {
      // need to know type :( .. what if not float?
      sum = Mutable:ref( gint(0) );
      for k = 0 to m2`length-1 {
	sum := sum + (Matrix:get(m1,i,k) * Matrix:get(m2,k,j));
      };
      Matrix:set(m3,i,j,sum)
    }
  };
  m3 // Return.
 }


 // [2007.07.12] These ape the structure of the matrix_gsl.ws for interoperability.
 namespace Float {
   fun create(i,j) Matrix:create(i,j, 0.0);
   eq        :: (Matrix Float, Matrix Float) -> Bool  = Matrix:eq;
   get       :: (Matrix Float, Int, Int) -> Float     = Matrix:get;
   set       :: (Matrix Float, Int, Int, Float) -> () = Matrix:set;
   dims      ::  Matrix Float -> (Int * Int)          = Matrix:dims;
   row       :: (Matrix Float, Int) -> Array Float    = Matrix:row;
   col       :: (Matrix Float, Int) -> Array Float    = Matrix:col;
   toArray   ::  Matrix Float -> Array Float          = Matrix:toArray;
   fromArray :: (Array Float, Int) -> Matrix Float    = Matrix:fromArray;
   fromArray2d  :: Array (Array Float) -> Matrix Float           = Matrix:fromArray2d;
   fromList2d   :: List  (List Float)  -> Matrix Float           = Matrix:fromList2d;

   add          :: (Matrix Float, Matrix Float) -> Matrix Float  = Matrix:add;   
   sub          :: (Matrix Float, Matrix Float) -> Matrix Float  = Matrix:sub;   
   mul_elements :: (Matrix Float, Matrix Float) -> Matrix Float  = Matrix:mul_elements;   
   div_elements :: (Matrix Float, Matrix Float) -> Matrix Float  = Matrix:div_elements;  
   scale        :: (Matrix Float, Float) -> Matrix Float         = Matrix:scale;   
   add_constant :: (Matrix Float, Float) -> Matrix Float         = Matrix:add_constant;   
   mul          :: (Matrix Float, Matrix Float) -> Matrix Float  = Matrix:mul;   

   add_inplace          :: (Matrix Float, Matrix Float) -> ()  = Matrix:add_inplace;   
   sub_inplace          :: (Matrix Float, Matrix Float) -> ()  = Matrix:sub_inplace;   
   mul_elements_inplace :: (Matrix Float, Matrix Float) -> ()  = Matrix:mul_elements_inplace; 
   div_elements_inplace :: (Matrix Float, Matrix Float) -> ()  = Matrix:div_elements_inplace;
   scale_inplace        :: (Matrix Float, Float) -> ()         = Matrix:scale_inplace;
   add_constant_inplace :: (Matrix Float, Float) -> ()         = Matrix:add_constant_inplace;

   build        :: (Int, Int, (Int, Int) -> Float) -> Matrix Float = Matrix:build;
   foreachi     :: ((Int, Int, Float) -> (), Matrix Float) -> ()   = Matrix:foreachi;
   rowmap       ::  (Array Float -> b, Matrix Float) -> Array b    = Matrix:rowmap;
   map          ::  (Float -> Float, Matrix Float) -> Matrix Float = Matrix:map;
   map_inplace  ::  (Float -> Float, Matrix Float) -> ()           = Matrix:map_inplace;
   map2         :: ((Float,Float) -> Float, Matrix Float, Matrix Float) -> Matrix Float = Matrix:map2;
   map2_inplace :: ((Float,Float) -> Float, Matrix Float, Matrix Float) -> ()   = Matrix:map2_inplace;
 }


 // The below namespaces are cut/paste from "Float" above.
 namespace Double {
   eq        :: (Matrix Double, Matrix Double) -> Bool  = Matrix:eq;
   get       :: (Matrix Double, Int, Int) -> Double     = Matrix:get;
   set       :: (Matrix Double, Int, Int, Double) -> () = Matrix:set;
   dims      ::  Matrix Double -> (Int * Int)          = Matrix:dims;
   row       :: (Matrix Double, Int) -> Array Double    = Matrix:row;
   col       :: (Matrix Double, Int) -> Array Double    = Matrix:col;
   toArray   ::  Matrix Double -> Array Double          = Matrix:toArray;
   fromArray :: (Array Double, Int) -> Matrix Double    = Matrix:fromArray;
   fromArray2d  :: Array (Array Double) -> Matrix Double           = Matrix:fromArray2d;
   fromList2d   :: List  (List Double)  -> Matrix Double           = Matrix:fromList2d;

   add          :: (Matrix Double, Matrix Double) -> Matrix Double  = Matrix:add;   
   sub          :: (Matrix Double, Matrix Double) -> Matrix Double  = Matrix:sub;   
   mul_elements :: (Matrix Double, Matrix Double) -> Matrix Double  = Matrix:mul_elements;   
   div_elements :: (Matrix Double, Matrix Double) -> Matrix Double  = Matrix:div_elements;  
   scale        :: (Matrix Double, Double) -> Matrix Double         = Matrix:scale;   
   add_constant :: (Matrix Double, Double) -> Matrix Double         = Matrix:add_constant;   
   mul          :: (Matrix Double, Matrix Double) -> Matrix Double  = Matrix:mul;   

   add_inplace          :: (Matrix Double, Matrix Double) -> ()  = Matrix:add_inplace;   
   sub_inplace          :: (Matrix Double, Matrix Double) -> ()  = Matrix:sub_inplace;   
   mul_elements_inplace :: (Matrix Double, Matrix Double) -> ()  = Matrix:mul_elements_inplace; 
   div_elements_inplace :: (Matrix Double, Matrix Double) -> ()  = Matrix:div_elements_inplace;
   scale_inplace        :: (Matrix Double, Double) -> ()         = Matrix:scale_inplace;
   add_constant_inplace :: (Matrix Double, Double) -> ()         = Matrix:add_constant_inplace;

   build        :: (Int, Int, (Int, Int) -> Double) -> Matrix Double = Matrix:build;
   foreachi     :: ((Int, Int, Double) -> (), Matrix Double) -> ()   = Matrix:foreachi;
   rowmap       ::  (Array Double -> b, Matrix Double) -> Array b    = Matrix:rowmap;
   map          ::  (Double -> Double, Matrix Double) -> Matrix Double = Matrix:map;
   map_inplace  ::  (Double -> Double, Matrix Double) -> ()           = Matrix:map_inplace;
   map2         :: ((Double,Double) -> Double, Matrix Double, Matrix Double) -> Matrix Double = Matrix:map2;
   map2_inplace :: ((Double,Double) -> Double, Matrix Double, Matrix Double) -> ()   = Matrix:map2_inplace;
 }


 namespace Complex {
   eq        :: (Matrix Complex, Matrix Complex) -> Bool  = Matrix:eq;
   get       :: (Matrix Complex, Int, Int) -> Complex     = Matrix:get;
   set       :: (Matrix Complex, Int, Int, Complex) -> () = Matrix:set;
   dims      ::  Matrix Complex -> (Int * Int)          = Matrix:dims;
   row       :: (Matrix Complex, Int) -> Array Complex    = Matrix:row;
   col       :: (Matrix Complex, Int) -> Array Complex    = Matrix:col;
   toArray   ::  Matrix Complex -> Array Complex          = Matrix:toArray;
   fromArray :: (Array Complex, Int) -> Matrix Complex    = Matrix:fromArray;
   fromArray2d  :: Array (Array Complex) -> Matrix Complex           = Matrix:fromArray2d;
   fromList2d   :: List  (List Complex)  -> Matrix Complex           = Matrix:fromList2d;

   add          :: (Matrix Complex, Matrix Complex) -> Matrix Complex  = Matrix:add;   
   sub          :: (Matrix Complex, Matrix Complex) -> Matrix Complex  = Matrix:sub;   
   mul_elements :: (Matrix Complex, Matrix Complex) -> Matrix Complex  = Matrix:mul_elements;   
   div_elements :: (Matrix Complex, Matrix Complex) -> Matrix Complex  = Matrix:div_elements;  
   scale        :: (Matrix Complex, Complex) -> Matrix Complex         = Matrix:scale;   
   add_constant :: (Matrix Complex, Complex) -> Matrix Complex         = Matrix:add_constant;   
   mul          :: (Matrix Complex, Matrix Complex) -> Matrix Complex  = Matrix:mul;   

   add_inplace          :: (Matrix Complex, Matrix Complex) -> ()  = Matrix:add_inplace;   
   sub_inplace          :: (Matrix Complex, Matrix Complex) -> ()  = Matrix:sub_inplace;   
   mul_elements_inplace :: (Matrix Complex, Matrix Complex) -> ()  = Matrix:mul_elements_inplace; 
   div_elements_inplace :: (Matrix Complex, Matrix Complex) -> ()  = Matrix:div_elements_inplace;
   scale_inplace        :: (Matrix Complex, Complex) -> ()         = Matrix:scale_inplace;
   add_constant_inplace :: (Matrix Complex, Complex) -> ()         = Matrix:add_constant_inplace;

   build        :: (Int, Int, (Int, Int) -> Complex) -> Matrix Complex = Matrix:build;
   foreachi     :: ((Int, Int, Complex) -> (), Matrix Complex) -> ()   = Matrix:foreachi;
   rowmap       ::  (Array Complex -> b, Matrix Complex) -> Array b    = Matrix:rowmap;
   map          ::  (Complex -> Complex, Matrix Complex) -> Matrix Complex = Matrix:map;
   map_inplace  ::  (Complex -> Complex, Matrix Complex) -> ()           = Matrix:map_inplace;
   map2         :: ((Complex,Complex) -> Complex, Matrix Complex, Matrix Complex) -> Matrix Complex = Matrix:map2;
   map2_inplace :: ((Complex,Complex) -> Complex, Matrix Complex, Matrix Complex) -> ()   = Matrix:map2_inplace;
 }


 // Except this bit:
 fun Double:create(i,j) Matrix:create(i,j, floatToDouble(0.0));
 fun Complex:create(i,j) Matrix:create(i,j, 0.0+0.0i);

  
};

