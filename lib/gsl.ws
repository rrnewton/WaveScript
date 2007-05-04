
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

nullperm :: Int -> Pointer
  = foreign "makeNullPerm" in gsl_includes
//    in List:append(gsl_includes, [GETENV("REGIMENTD") ++ "/lib/GSL_extras.c"])

/*==============================================================================*/
/*                             Matrix functions:                                */
/*==============================================================================*/

// Takes matrix_in, permutation, matrix_out
gsl_linalg_complex_LU_invert :: (Array Complex, GslPerm, Array Complex) -> Int
  = foreign "gsl_linalg_complex_LU_invert" in gsl_includes


/*==============================================================================*/
/*==============================================================================*/

BASE <- iterate _ in timer(30.0) { emit nullperm(30) }
