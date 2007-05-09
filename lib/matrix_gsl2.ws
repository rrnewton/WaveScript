
// In this version, we're exploring the concept of an "ExclusivePointer" type.

// Should use C++ preprocessor to generate all the variants.

include "gsl.ws";

// A pair containing the struct pointer and the array pointer.
type Matrix #n = (ExclusivePointer * ExclusivePointer);

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
  
    // Hmm... initialize how?
    fun create(n,m) {  
      p   = exclusivePtr $ gsl_matrix_float_alloc(n,m);
      arr = exclusivePtr $ gsl_matrix_float_data(getPtr(p));
      gsl_matrix_float_set_zero(p`getPtr);
      (p, arr)
    }

    // These could be implemented by directly using the array:
    fun get((mat,_),i,j)    gsl_matrix_float_get(mat`getPtr, i,j)
    fun set((mat,_),i,j,x)  gsl_matrix_float_set(mat`getPtr, i,j, x)
    fun dims((mat,_)) {
      let x = gsl_matrix_float_size1(mat`getPtr);
      let y = gsl_matrix_float_size2(mat`getPtr);
       (x,y)
    }

  }


  // We don't support complex numbers in the FFI yet! 
  namespace Complex {
    fun create(n,m) {  
      p   = exclusivePtr $ gsl_matrix_complex_float_alloc(n,m);
      arr = exclusivePtr $ gsl_matrix_complex_float_data(getPtr(p));
      gsl_matrix_complex_float_set_zero(p`getPtr);
      (p, arr)
    }

    fun dims((mat,_)) {
      let x = gsl_matrix_complex_float_size1(mat`getPtr);
      let y = gsl_matrix_complex_float_size2(mat`getPtr);
       (x,y)
    }

    fun invert((m1,d1)) {
      let (x,y)   = dims((m1,d1));
      let (m2,d2) = create(x,y);
      let perm = nullperm(x);
        // Do the work:
        gsl_linalg_complex_float_LU_invert(m1`getPtr, perm, m2`getPtr);
	Cfree(perm);
      (m2,d2)
    }
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
