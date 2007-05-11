
// In this version, we're exploring the concept of an "ExclusivePointer" type.

// Should use C++ preprocessor to generate all the variants.

include "gsl.ws";

// A pair containing the struct pointer and the array pointer.
type Matrix #n = (ExclusivePointer * ExclusivePointer);

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
    };


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

  /*
  fun create(n,m, init) {
    typecase init {
      Int : {
        ptr = gsl_matrix_alloc(n,m);
	//managed_ptr = makeExclusive(ptr, free);
				
      }	
      Float : {
      }
    }
    }*/

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
  print("Ref: "++ get(m,1,1)  ++"\n");
  set(m, 1,1, 3.0);
  print("  Set... \n");
  print("  Ref: "++ get(m,1,1)  ++"\n\n");
  emit m;
  //  emit m ` invert;
}
