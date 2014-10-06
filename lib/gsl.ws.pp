
/* ISSUES

What can we do about structs and other types that we don't want to interpret?
We can shove them into a "magic" type, can't we?
Specifically, a "Pointer" type.

*/


//gsl_includes = ["<gsl/gsl_linalg.h>", "<gsl/gsl_matrix.h>", 
//                "libm.so", "libgslcblas.so", "libgsl.so"]

// I have to pre-link them because of messed up undefined symbol errors:
gsl_includes = ["gsl/gsl_linalg.h", "gsl/gsl_matrix.h", "GSL_extras.h", 
                GETENV("WAVESCRIPTD")++ "/lib/ws_gslpak.so"]

// gsl_blas.ws -- The high level blas interface.

#define wrap(x) #x
#define entry(NAME,TYPE) NAME :: TYPE = foreign(wrap(NAME), gsl_includes);

#define ALL(OP) OP() OP(_float) OP(_complex_float) OP(_complex)
#define ALL2(OP) OP(, Double) OP(_float, Float) OP(_complex_float, Complex) /*OP(_complex, ComplexDouble)*/

//#define ALL(OP) OP(_float) 

//#define PTR2(S,STAR) Pointer wrap(gsl_matrix##S##STAR())
#define PTR(S) Pointer wrap(gsl_matrix##S *)

#define plussuffix(SUF,S1,T) entry(gsl_matrix##S1##_##SUF, T)


/*====================================================================================================*/
/*                                       RRN's Extra functions:                                       */
/*====================================================================================================*/

nullperm :: Int -> Pointer "gsl_permutation*" = foreign("makeNullPerm", gsl_includes)
//    in List:append(gsl_includes, [GETENV("WAVESCRIPTD") ++ "/lib/GSL_extras.c"])

nulltranspose :: () -> Int = foreign("makeCblasNoTrans", gsl_includes)

makeMatrixWrapper :: Array #n -> Pointer "gsl_matrix*" = foreign("makeNullPerm", gsl_includes)

// These return the array contained within the matrix struct:
// Actually, it's kind of silly for these to be different... they
// access the same memory location within the struct.  Only the data payload is different.
//#define data(S) entry(gsl_matrix##S##_data, Pointer -> Pointer)
#define data(S)  plussuffix( data, S, PTR(S) -> Pointer "void*")

ALL(data)

#define size1(S) plussuffix( size1, S, PTR(S) -> Int)
#define size2(S) plussuffix( size2, S, PTR(S) -> Int)

ALL(size1)
ALL(size2)

/*====================================================================================================*/
/*                                         GSL Matrix functions:                                      */
/*====================================================================================================*/

/// THIS IS ORGANIZED BY THE SECTION NUMBERS IN THE GSL DOCUMENTATION (VER 1.9).

// Section 8.4.1 - Complete

#define alloc(S)  plussuffix( alloc, S, (Int,Int) -> PTR(S))
#define calloc(S) plussuffix( calloc, S, (Int,Int) -> PTR(S))
#define free(S)   plussuffix( free,  S, (PTR(S)   -> ()))

ALL(alloc)
ALL(calloc)
ALL(free)

// Section 8.4.2 - Incomplete (_ptr and _const_ptr)
//  plus: Section 8.4.3 - Complete

#define getset(CTY,WSTY) \
   entry(gsl_matrix##CTY##_get,     (PTR(CTY), Int, Int) -> WSTY) \
   entry(gsl_matrix##CTY##_set,     (PTR(CTY), Int, Int, WSTY) -> ()) \
   entry(gsl_matrix##CTY##_set_all, (PTR(CTY), WSTY) -> ()) 

#define setconst(CTY,WSTY) \
   entry(gsl_matrix##CTY##_set_zero,     PTR(CTY) -> ()) \
   entry(gsl_matrix##CTY##_set_identity, PTR(CTY) -> ())

ALL2(setconst)

getset(, Double) 
getset(_float, Float) // Complex handled below...

// Section 8.4.4 - File Reading - Incomplete
// Section 8.4.5 - Matrix Views - Incomplete
// Section 8.4.6 - Row and Column views - Incomplete

// Section 8.4.7 - Complete

#define copy(S) plussuffix( memcpy, S, (PTR(S), PTR(S)) -> Int)
#define swap(S) plussuffix( swap,   S, (PTR(S), PTR(S)) -> Int)
ALL(copy)
ALL(swap)

// Section 8.4.8 - Copying rows & columns - Incomplete
// Section 8.4.9 - Exchanging rows & columns - Incomplete

// Section 8.4.10 - Complete

#define add(S) plussuffix( add,          S, (PTR(S), PTR(S)) -> Int)
#define sub(S) plussuffix( sub,          S, (PTR(S), PTR(S)) -> Int)
#define mul(S) plussuffix( mul_elements, S, (PTR(S), PTR(S)) -> Int)
#define div(S) plussuffix( div_elements, S, (PTR(S), PTR(S)) -> Int)
#define scale(CTY,WSTY)    plussuffix( scale,        CTY, (PTR(CTY), WSTY) -> Int)
#define addconst(CTY,WSTY) plussuffix( add_constant, CTY, (PTR(CTY), WSTY) -> Int)

ALL(add)
ALL(sub)
ALL(mul)
ALL(div)
scale(, Double)    scale(_float, Float)
addconst(, Double) addconst(_float, Float) // Complex handled below...


// Section 8.4.11 - Finding max & min - Incomplete

// Section 8.4.12 - Complete

#define isnull(S) plussuffix( isnull,          S, PTR(S) -> Int)
#define ispos(S)  plussuffix( ispos,           S, PTR(S) -> Int)
#define isneg(S)  plussuffix( isneg,           S, PTR(S) -> Int)
ALL(isnull)
ALL(ispos)
ALL(isneg)

/*====================================================================================================*/
/*                                  Routines with Complex Arguments                                   */
/*====================================================================================================*/

// These have to be defined specially for complex, because Scheme and MLton
// FFI's can't pass complex numbers.
get_complex_ptr_real :: Pointer "wscomplex_t" -> Float = foreign("get_complex_ptr_real",gsl_includes);
get_complex_ptr_imag :: Pointer "wscomplex_t" -> Float = foreign("get_complex_ptr_imag",gsl_includes);
ws_gsl_matrix_complex_float_get = (foreign("ws_gsl_matrix_complex_float_get", gsl_includes) 
              :: (Pointer "gsl_matrix_complex_float*", Int, Int) -> Pointer "wscomplex_t");
ws_gsl_matrix_complex_float_set = (foreign("ws_gsl_matrix_complex_float_set", gsl_includes) 
              :: (Pointer "gsl_matrix_complex_float*", Int, Int, Float, Float) -> ());

ws_gsl_matrix_complex_float_scale = (foreign("ws_gsl_matrix_complex_float_scale", gsl_includes)
              :: (Pointer "gsl_matrix_complex_float*", Float,Float) -> ());
ws_gsl_matrix_complex_float_add_constant = (foreign("ws_gsl_matrix_complex_float_add_constant", gsl_includes)
              :: (Pointer "gsl_matrix_complex_float*", Float,Float) -> ());

fun gsl_matrix_complex_float_get(mat, i,j) {
  p = ws_gsl_matrix_complex_float_get(mat,i,j);
  makeComplex(get_complex_ptr_real(p), get_complex_ptr_imag(p));
}
fun gsl_matrix_complex_float_set(mat, i,j, c) 
  ws_gsl_matrix_complex_float_set(mat,i,j, realpart(c), imagpart(c));

fun gsl_matrix_complex_float_scale(mat, c)
  ws_gsl_matrix_complex_float_scale(mat, realpart(c), imagpart(c));
fun gsl_matrix_complex_float_add_constant(mat, c)
  ws_gsl_matrix_complex_float_add_constant(mat, realpart(c), imagpart(c));


/*====================================================================================================*/
/*                                         GSL Linear Algebra                                         */
/*====================================================================================================*/

// Section 13.1 - Incomplete

// Inversion: Takes matrix_in, permutation, matrix_out
#define invert(S) entry(gsl_linalg##S##_LU_invert, (PTR(S), PTR(S), PTR(S)) -> Int)

invert()
//invert(_float)  // I don't think this exists!!
invert(_complex)
invert(_complex_float)


/*====================================================================================================*/
/*                                         GSL BLAS routines                                          */
/*====================================================================================================*/


// DANGER: The first argument is an ENUM, not necessarily the sizeof an int.
gsl_blas_sgemm = (foreign("gsl_blas_sgemm", gsl_includes) ::
  (Int, Int, Float, Pointer "gsl_matrix_float*", Pointer "gsl_matrix_float*", Float, Pointer "gsl_matrix_float*") -> Int);

gsl_blas_dgemm = (foreign("gsl_blas_dgemm", gsl_includes) ::
  (Int, Int, Double, Pointer "gsl_matrix*", Pointer "gsl_matrix*", Double, Pointer "gsl_matrix*") -> Int);

/*
gsl_matrix_get :: (Pointer "gsl_matrix *", Int, Int) -> Double = foreign("gsl_matrix_get", gsl_includes);
gsl_matrix_add :: (Pointer "gsl_matrix *", Pointer "gsl_matrix *") -> Int = foreign("gsl_matrix_add", gsl_includes);
#define add(S) plussuffix( add,          S, (PTR(S), PTR(S)) -> Int)
ALL(add)

int  gsl_blas_sgemm (CBLAS_TRANSPOSE_t TransA,
                     CBLAS_TRANSPOSE_t TransB,
                     float alpha,
                     const gsl_matrix_float * A,
                     const gsl_matrix_float * B,
                     float beta,
                     gsl_matrix_float * C);
*/


/*====================================================================================================*/

// This only works for linux:
/* libc = if SHELL("uname") == "Darwin\n" */
/*        then ["libc.dylib"] */
/*        else ["libc.so.6"]; */

// [2007.09.14] CHANGE!  Now libc is IMPLICITELY included in the foreign interface:
libc = [];
Cfree :: Pointer "void*" -> () = foreign("free", libc)

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


/*

{
  double A[] = { 
    1., 2., 
    3., 4. 
  };
  double B[] = {
    2., 3.,
    4., 5.
  };
  double alpha = 1., beta = 0.;

  gsl_matrix_view A_m = gsl_matrix_view_array(A, 2, 2);
  gsl_matrix_view B_m = gsl_matrix_view_array(B, 2, 2);
  gsl_matrix *C = gsl_matrix_alloc(2,2);

  gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, alpha, &A_m.matrix, 
      &B_m.matrix, beta, C);

  printf("[[\n%f\t%f\n%f\t%f]]\n", 
      gsl_matrix_get(C, 0, 0), gsl_matrix_get(C, 0, 1), 
      gsl_matrix_get(C, 1, 0), gsl_matrix_get(C, 1, 1));

  gsl_matrix_free(C);
  return 0;
}

*/
