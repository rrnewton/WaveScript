
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
#define entry(NAME,TYPE) NAME :: TYPE = foreign(wrap(NAME), gsl_includes);

#define ALL(OP) OP() OP(_float) OP(_complex_float) OP(_complex)
//#define ALL(OP) OP(_float) 

#define plussuffix(SUF,S1,T) entry(gsl_matrix##S1##_##SUF, T)


/*====================================================================================================*/
/*                                       RRN's Extra functions:                                       */
/*====================================================================================================*/

nullperm :: Int -> Pointer = foreign("makeNullPerm", gsl_includes)
//    in List:append(gsl_includes, [GETENV("REGIMENTD") ++ "/lib/GSL_extras.c"])

makeMatrixWrapper :: Array #n -> Pointer = foreign("makeNullPerm", gsl_includes)

// These return the array contained within the matrix struct:
// Actually, it's kind of silly for these to be different... they
// access the same memory location within the struct.  Only the data payload is different.
//#define data(S) entry(gsl_matrix##S##_data, Pointer -> Pointer)
#define data(S)  plussuffix( data, S, Pointer -> Pointer)

ALL(data)

#define size1(S) plussuffix( size1, S, Pointer -> Int)
#define size2(S) plussuffix( size2, S, Pointer -> Int)

ALL(size1)
ALL(size2)


/*====================================================================================================*/
/*                                         GSL Matrix functions:                                      */
/*====================================================================================================*/

#define alloc(S) plussuffix( alloc, S, (Int,Int) -> Pointer)
#define free(S)  plussuffix( free,  S, (Pointer -> ()))

ALL(alloc)
ALL(free)


#define getset(CTY,WSTY) \
   entry(gsl_matrix##CTY##_get,     (Pointer, Int, Int) -> WSTY) \
   entry(gsl_matrix##CTY##_set,     (Pointer, Int, Int, WSTY) -> ()) \
   entry(gsl_matrix##CTY##_set_all, (Pointer, WSTY) -> ()) \
   entry(gsl_matrix##CTY##_set_zero, Pointer -> ())


//getset(, Double)  // Not ready yet
getset(_float, Float)
getset(_complex_float, Complex)
//getset(_complex, ComplexDouble)

// Inversion: Takes matrix_in, permutation, matrix_out
#define invert(S) entry(gsl_linalg##S##_LU_invert, (Pointer, Pointer, Pointer) -> Int)

invert()
//invert(_float)  // I don't think this exists!!
invert(_complex)
invert(_complex_float)


/*====================================================================================================*/

// This only works for linux:
Cfree :: Pointer -> () = foreign("free", ["libc.so.6"])

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
