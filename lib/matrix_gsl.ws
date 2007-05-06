include "gsl.ws";

// This wraps a cleaner interfaces to the matrix operations inside the Gnu Scientific Librray.

/*
     typedef struct
     {
       size_t size1;
       size_t size2;
       size_t tda;
       double * data;
       gsl_block * block;
       int owner;
     } gsl_matrix;
*/


type Matrix #n = (Int * Int * Array #n);

namespace Matrix {

  fun create(n,m, init) (n,m, Array:make(n*m, init))

  fun invert((n,m,dat)) {
    let (_,_,out) = create(n,m,gint(0));
    m1 = makeMatrixWrapper(dat);
    m2 = makeMatrixWrapper(out);
    perm = nullperm(n);
    // Do the work:
    gsl_linalg_complex_LU_invert(m1, perm, m2);
    free(m1); free(m2);
    (n,m,out)
  }

 /*  
 
 typecase dat {
   Array Complex |
   Array Float   |
 }


 */


 /*  
 fun invert(mat) {
  out = create(n,m);
  gsl_linalg_complex_LU_invert(mat, nullPerm, out);
 }     
 */ 

}
