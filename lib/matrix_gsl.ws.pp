
// In this version, we're exploring the concept of an "ExclusivePointer" type.

// Should use C++ preprocessor to generate all the variants.

include "gsl.ws";

// Here's our lame enumeration... We need union types.
float_matrix  = 0;
double_matrix = 1;
complex_matrix = 2;
complexdouble_matrix = 3;

// A pair containing the struct pointer and the array pointer.
type MatrixTypeTag = Int;

type Matrix = (Int * ExclusivePointer "void*" * ExclusivePointer "void*");
//type Matrix #n = (ExclusivePointer "void*" * ExclusivePointer "void*");

#define BASIC(CTY, WSTY, TAG)         \
    /* Hmm... initialize how? */ \
    create :: (Int,Int) -> Matrix; \
    fun create(n,m) {  \
      p   = exclusivePtr $ gsl_matrix##CTY##_alloc(n,m); \
      arr = exclusivePtr $ gsl_matrix##CTY##_data(getPtr(p)); \
      gsl_matrix##CTY##_set_zero(p`getPtr); \
      (TAG, p, arr) \
    } \
      \
    /* These could be implemented by directly using the array: */ \
    get  :: (Matrix, Int, Int)       -> WSTY; \
    set  :: (Matrix, Int, Int, WSTY) -> ();   \
    dims :: (Matrix)                 -> (Int * Int); \
    fun get((_,mat,_),i,j)    gsl_matrix##CTY##_get(mat`getPtr, i,j); \
    fun set((_,mat,_),i,j,x)  gsl_matrix##CTY##_set(mat`getPtr, i,j, x); \
    fun dims((_,mat,_)) { \
      let x = gsl_matrix##CTY##_size1(mat`getPtr); \
      let y = gsl_matrix##CTY##_size2(mat`getPtr); \
       (x,y) \
    }; \
       \
    /* This is temporary, we can't pass arrays yet... but we can do this */ \
    fun toArray(mat) {             \
      let (x,y) = dims(mat);       \
      arr = Array:makeUNSAFE(x*y); \
      for j = 0 to y-1 {           \
        for i = 0 to x-1 {         \
	  Array:set(arr, i + j*x, get(mat,i,j)) \
	} \
      };  \
      arr \
    } \
    fun fromArray(arr, rowlen) {   \
      len = Array:length(arr);     \
      rows = len / rowlen;        \
      if (len != rowlen * rows)    \
      then wserror("fromArray: array length "++ len ++" is not divisible by "++ rowlen ++". Cannot convert to matrix."); \
      mat = create(rowlen, rows);  \
      for j = 0 to rows-1 {           \
        for i = 0 to rowlen-1 {         \
	  set(mat, i, j, arr[i + j*rowlen]) \
	} \
      };  \
      mat \
    } \


// One day we could do this with type classes.
#define INVERT(CTY, WSTY)          \
    fun invert(mat) {          \
      let (tag,m1,d1) = mat;   \
      let (x,y)   = dims(mat); \
      let mat2 = create(x,y);   \
      let (tag,m2,d2) = mat2;   \
      let perm = nullperm(x);      \
        /* Do the work: */         \
        gsl_linalg##CTY##_LU_invert(m1`getPtr, perm, m2`getPtr); \
	Cfree(perm);               \
	mat2 \
    }


namespace Matrix {

  namespace Float {
    BASIC(_float, Float, float_matrix)
   //INVERT()  // Apparently not implemented for single precision...
  }

  namespace Double {
    BASIC(, Double, double_matrix)
   INVERT(, Double)
  }

  // We don't support complex numbers in the FFI yet! 
  namespace Complex {
    BASIC(_complex_float, Complex, complex_matrix)
//    INVERT(_complex_float, Complex) // WHY IS THIS NOT DEFINED?
  }

  //  namespace ComplexDouble {} 

  // A generic inversion routine:
  fun invert(mat) {
    let (tag,_,_) = mat;
    //if tag == float_matrix
    //then Float:invert(mat) else
    if tag == double_matrix
    then Double:invert(mat) else
//    if tag == complex_matrix
//    then Complex:invert(mat) else
//    if tag == complexdouble_matrix
//    then ComplexDouble:invert(mat)
    wserror("Unrecognized matrix type tag: "++tag)
  }
  
}

BASE <- iterate _ in timer(30.0) 
{ 
  using Matrix; using Float;

  m = create(2,3);
  print("Ref: "++ get(m,0,0)  ++"\n");
  print("Ref: "++ get(m,1,2)  ++"\n");
  set(m, 1,2, 3.0);
  set(m, 1,1, 9.0);
  print("  Set... \n");
  print("  Ref: "++ get(m,1,2)  ++"\n");
  arr = m ` toArray;
  print(" Converted to array: "++ arr ++"\n");
  mat2 = fromArray(arr,2);
  print(" Converted back to matrix: "++ mat2 ++"\n");
  print(" And back to array : "++ mat2 ` toArray ++"\n\n");

  using Matrix; using Double;

  dub = create(3,3);
  set(dub, 0,0, floatToDouble$ 1.0);
  set(dub, 1,1, floatToDouble$ 2.0);
  set(dub, 2,2, floatToDouble$ 3.0);
  
  print("A double matrix    : "++ dub `toArray ++"\n");
  inv = Matrix:invert(dub);
  print("It's inverse       : "++ inv `toArray ++"\n");
  print("It's double inverse: "++ inv `Matrix:invert `toArray ++"\n");
  
  emit m;
  emit dub;
  emit Matrix:Complex:create(3,3);
  //  emit m ` invert;
}
