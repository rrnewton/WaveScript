include "gsl.ws";

// This wraps a cleaner interfaces to the matrix operations inside the Gnu Scientific Librray.

namespace Matrix {

 fun create(n,m) {
      
 }
  
 fun invert(mat) {
  out = create(n,m);
  gsl_linalg_complex_LU_invert(mat, nullPerm, out);
 }     
 
}
