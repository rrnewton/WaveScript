
/* ISSUES

What can we do about structs and other types that we don't want to interpret?
We can shove them into a "magic" type, can't we?
Specifically, a "Pointer" type.

*/


//gsl_includes = ["<gsl/gsl_linalg.h>", "<gsl/gsl_matrix.h>", 
//                "libm.so", "libgslcblas.so", "libgsl.so"]

// I have to pre-link them because of messed up undefined symbol errors:
gsl_includes = ["gsl/gsl_linalg.h", "gsl/gsl_matrix.h", "GSL_extras.h", 
                GETENV("REGIMENTD")++ "/lib/ws_gslpak.so"]

//type GslPerm = (Int, Array Int);
type GslPerm = Pointer;


/*====================================================================================================*/
/*                                       RRN's Extra functions:                                       */
/*====================================================================================================*/

nullperm :: Int -> Pointer = foreign "makeNullPerm" in gsl_includes
//    in List:append(gsl_includes, [GETENV("REGIMENTD") ++ "/lib/GSL_extras.c"])

makeMatrixWrapper :: Array #n -> Pointer = foreign "makeNullPerm" in gsl_includes

// These return the array contained within the matrix struct:
gsl_matrix_data       :: Pointer -> Pointer = foreign "gsl_matrix_data"        in gsl_includes
gsl_matrix_float_data :: Pointer -> Pointer = foreign "gsl_matrix_float_data"  in gsl_includes

gsl_matrix_size1         :: Pointer -> Int    = foreign "gsl_matrix_size1"        in gsl_includes
gsl_matrix_size2         :: Pointer -> Int    = foreign "gsl_matrix_size2"        in gsl_includes
gsl_matrix_float_size1   :: Pointer -> Int    = foreign "gsl_matrix_float_size1"  in gsl_includes
gsl_matrix_float_size2   :: Pointer -> Int    = foreign "gsl_matrix_float_size2"  in gsl_includes
gsl_matrix_complex_size1 :: Pointer -> Int    = foreign "gsl_matrix_complex_size1"  in gsl_includes
gsl_matrix_complex_size2 :: Pointer -> Int    = foreign "gsl_matrix_complex_size2"  in gsl_includes


/*====================================================================================================*/
/*                                         Matrix functions:                                          */
/*====================================================================================================*/

gsl_matrix_alloc       :: (Int,Int) -> Pointer = foreign "gsl_matrix_alloc"       in gsl_includes
gsl_matrix_float_alloc :: (Int,Int) -> Pointer = foreign "gsl_matrix_float_alloc" in gsl_includes

gsl_matrix_free       :: Pointer -> () = foreign "gsl_matrix_free"       in gsl_includes
gsl_matrix_float_free :: Pointer -> () = foreign "gsl_matrix_float_free" in gsl_includes

// Takes matrix_in, permutation, matrix_out
gsl_linalg_LU_invert :: (Pointer, Pointer, Pointer) -> Int
    = foreign "gsl_linalg_LU_invert" in gsl_includes

gsl_linalg_complex_LU_invert :: (Pointer, Pointer, Pointer) -> Int
    = foreign "gsl_linalg_complex_LU_invert" in gsl_includes
gsl_linalg_complex_float_LU_invert :: (Pointer, Pointer, Pointer) -> Int
    = foreign "gsl_linalg_complex_float_LU_invert" in gsl_includes

// I don't think this exists!!
//gsl_linalg_float_LU_invert :: (Pointer, Pointer, Pointer) -> Int
//    = foreign "gsl_linalg_float_LU_invert" in gsl_includes


gsl_matrix_float_get  :: (Pointer, Int, Int) -> Float     = foreign "gsl_matrix_float_get"  in gsl_includes
gsl_matrix_float_set  :: (Pointer, Int, Int, Float) -> () = foreign "gsl_matrix_float_set"  in gsl_includes

gsl_matrix_float_set_all  :: (Pointer, Float) -> () = foreign "gsl_matrix_float_set_all"  in gsl_includes
gsl_matrix_float_set_zero :: Pointer -> ()          = foreign "gsl_matrix_float_set_zero" in gsl_includes

//gsl_matrix_set_all  :: Pointer, Double -> ()      = foreign "gsl_matrix_set_all"  in gsl_includes
gsl_matrix_set_zero :: Pointer -> ()                = foreign "gsl_matrix_set_zero" in gsl_includes


/*====================================================================================================*/

// This only works for linux:
Cfree :: Pointer -> () = foreign "free" in ["libc.so.6"]

/*====================================================================================================*/


ignored = 
//BASE <- 
iterate _ in timer(30.0) 
{ 
  //m = gsl_matrix_alloc(2,3);
  //print("Matrix allocated\n");
  //print("Matrix allocated: "++ m ++"\n");
  //gsl_matrix_free(m);
  //emit nullperm(30);
  //  emit 3;
  //p = gsl_matrix_alloc(1000,1000);
  //p = gsl_matrix_alloc(3,3);
  p = exclusivePtr( gsl_matrix_alloc(3,3));
  emit (getPtr(p), gsl_matrix_data( getPtr(p)));
}
