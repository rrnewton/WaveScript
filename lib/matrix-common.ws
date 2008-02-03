
// Common parts of matrix lib

// NOTE! matrices are indixed (row,column)
// Remember to iterate over the matrix by incrementing row on the
// outside and column in the inner loop.

// Author:  Lewis Girod & Ryan Newton 

namespace Matrix {

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
   build(r,c, fun(i,j) f(get(mat,i,j)))
 }
 fun map2(f, mat1,mat2) {
   let (r,c) = Matrix:dims(mat1);
   build(r,c, fun(i,j) f(get(mat1,i,j), get(mat2,i,j)))
 }

 fun show2(mat) {
   let (r,c) = Matrix:dims(mat);
   str = Mutable:ref("");
   str := str ++ "[ ";
   for i = 0 to r - 1 {
     for j = 0 to c - 1 {
       str := str++Matrix:get(mat,i,j)++" ";
     };
     if (i != r-1) then 
       str := str ++ "\n  ";
   };
   str ++ "]\n"
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

 // Transpose
 fun trans(m) {
   let (r,c) = Matrix:dims(m);
   build(c,r,fun (j,i) get(m,i,j))
 }

 // Matrix multiplication.
 fun mul(m1,m2) {
  using Array;
  let (r1,c1) = Matrix:dims(m1);
  let (r2,c2) = Matrix:dims(m2);
  // TODO: could be more defensive here, check for null:
  m3 = Matrix:create(r1, c2, Matrix:get(m1,0,0));
  for i = 0 to r1-1 {
    for j = 0 to c2-1 {
      sum = Mutable:ref( gint(0) );
      for k = 0 to r2-1 {
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


