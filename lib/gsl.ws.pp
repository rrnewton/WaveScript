
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

#define wrap(x) #x
#define entry(NAME,TYPE) NAME :: TYPE = foreign(wrap(NAME), gsl_includes, []);

#define ALL(OP) OP() OP(_float) OP(_complex_float) OP(_complex)
#define ALL2(OP) OP(, Double) OP(_float, Float) OP(_complex_float, Complex) /*OP(_complex, ComplexDouble)*/

//#define ALL(OP) OP(_float) 

//#define PTR2(S,STAR) Pointer wrap(gsl_matrix##S##STAR())
#define PTR(S) Pointer wrap(gsl_matrix##S *)

#define plussuffix(SUF,S1,T) entry(gsl_matrix##S1##_##SUF, T)


/*====================================================================================================*/
/*                                       RRN's Extra functions:                                       */
/*====================================================================================================*/

nullperm :: Int -> Pointer "gsl_permutation*" = foreign("makeNullPerm", gsl_includes, [])
//    in List:append(gsl_includes, [GETENV("REGIMENTD") ++ "/lib/GSL_extras.c"])

makeMatrixWrapper :: Array #n -> Pointer "gsl_matrix*" = foreign("makeNullPerm", gsl_includes, [])

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
   entry(gsl_matrix##CTY##_set_all, (PTR(CTY), WSTY) -> ()) \
   entry(gsl_matrix##CTY##_set_zero,     PTR(CTY) -> ()) \
   entry(gsl_matrix##CTY##_set_identity, PTR(CTY) -> ())

ALL2(getset)

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
ALL2(scale)
ALL2(addconst)

// Section 8.4.11 - Finding max & min - Incomplete

// Section 8.4.12 - Complete

#define isnull(S) plussuffix( isnull,          S, PTR(S) -> Int)
#define ispos(S)  plussuffix( ispos,           S, PTR(S) -> Int)
#define isneg(S)  plussuffix( isneg,           S, PTR(S) -> Int)
ALL(isnull)
ALL(ispos)
ALL(isneg)

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

// This only works for linux:
Cfree :: Pointer "void*" -> () = foreign("free", ["libc.so.6"], [])

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
