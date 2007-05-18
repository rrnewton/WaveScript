
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

//type Matrix #n = (MatrixTypeTag * ExclusivePointer "void*" * ExclusivePointer "void*");
type Matrix #n = (ExclusivePointer "void*" * ExclusivePointer "void*");

#define BASIC(CTY, WSTY)         \
    /* Hmm... initialize how? */ \
    create :: (Int,Int) -> Matrix WSTY; \
    fun create(n,m) {  \
      p   = exclusivePtr $ gsl_matrix##CTY##_alloc(n,m); \
      arr = exclusivePtr $ gsl_matrix##CTY##_data(getPtr(p)); \
      gsl_matrix##CTY##_set_zero(p`getPtr); \
      (p, arr) \
    } \
      \
    /* These could be implemented by directly using the array: */ \
    get  :: (Matrix WSTY, Int, Int)       -> WSTY; \
    set  :: (Matrix WSTY, Int, Int, WSTY) -> ();   \
    dims :: (Matrix WSTY)                 -> (Int * Int); \
    fun get((mat,_),i,j)    gsl_matrix##CTY##_get(mat`getPtr, i,j); \
    fun set((mat,_),i,j,x)  gsl_matrix##CTY##_set(mat`getPtr, i,j, x); \
    fun dims((mat,_)) { \
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
    fun invert((m1,d1)) {          \
      let (x,y)   = dims((m1,d1)); \
      let (m2,d2) = create(x,y);   \
      let perm = nullperm(x);      \
        /* Do the work: */         \
        gsl_linalg##CTY##_LU_invert(m1`getPtr, perm, m2`getPtr); \
	Cfree(perm);               \
      (m2,d2)                      \
    }


namespace Matrix {

  namespace Float {
    BASIC(_float, Float)

   //INVERT()  // Apparently not implemented for single precision...
  }


  // We don't support complex numbers in the FFI yet! 
  namespace Complex {

    BASIC(_complex_float, Complex)

    INVERT(_complex_float, Complex)
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

  emit m;
  //  emit m ` invert;
}
