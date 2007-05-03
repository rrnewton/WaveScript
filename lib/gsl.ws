
/* ISSUES

What can we do about structs and other types that we don't want to interpret?
We can shove them into a "magic" type, can't we?
Specifically, a "Pointer" type.

*/



gsl_includes = ["<gsl/gsl_linalg.h>", "<gsl/gsl_matrix.h>", 
                "libm.so", "libgsl.so", "libgslcblas.so"]

//type GslPerm = (Int, Array Int);
type GslPerm = Pointer;

nullperm = (foreign "makeNullPerm" in "GSL_extras.c" :: () -> Pointer)();

/*==============================================================================*/
/*                             Matrix functions:                                */
/*==============================================================================*/

// Takes matrix_in, permutation, matrix_out
gsl_linalg_complex_LU_invert :: (Array Complex, GslPerm, Array Complex) -> Int
  = foreign "gsl_linalg_complex_LU_invert" in gsl_includes



/*==============================================================================*/
/*==============================================================================*/
