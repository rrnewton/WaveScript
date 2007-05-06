
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

nullperm :: Int -> Pointer = foreign "makeNullPerm" in gsl_includes
//    in List:append(gsl_includes, [GETENV("REGIMENTD") ++ "/lib/GSL_extras.c"])

makeMatrixWrapper :: Array #n -> Pointer = foreign "makeNullPerm" in gsl_includes

// This only works for linux:
free :: Pointer -> () = foreign "free" in ["libc.so.6"]

/*==============================================================================*/
/*                             Matrix functions:                                */
/*==============================================================================*/

gsl_matrix_alloc :: (Int,Int) -> Pointer 
  = foreign "gsl_matrix_alloc" in gsl_includes

gsl_matrix_free :: Pointer -> () = foreign "gsl_matrix_free" in gsl_includes

// Takes matrix_in, permutation, matrix_out
gsl_linalg_complex_LU_invert :: (Pointer, Pointer, Pointer) -> Int
  = foreign "gsl_linalg_complex_LU_invert" in gsl_includes


/*==============================================================================*/
/*==============================================================================*/

BASE <- iterate _ in timer(30.0) 
{ 
  //m = gsl_matrix_alloc(2,3);
  //print("Matrix allocated\n");
  //print("Matrix allocated: "++ m ++"\n");
  //gsl_matrix_free(m);
  emit nullperm(30);
  //  emit 3;
}
