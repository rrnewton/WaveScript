


//#include <gsl/gsl_linalg.h>
//#include <gsl/gsl_matrix.h>

#include "gsl/gsl_linalg.h"
#include "gsl/gsl_matrix.h"

// This makes an identity permutation
void* makeNullPerm(int size) {
  gsl_permutation* perm = gsl_permutation_calloc(size);
  return perm;
}

// Builds a gsl_matrix wrapper.
// This should actually work for gsl_matrix_complex, gsl_matrix_float, or gsl_matrix.
// (Same memory layout.)
void* makeMatrixWrapper(int x, int y, void* data) {
  gsl_matrix_float* w = (gsl_matrix_float*)malloc(sizeof(gsl_matrix_float));
  w->size1 = x;    
  w->size2 = y;
  w->tda   = x;
  w->block = 0;
  w->owner = 0;
  w->data  = (float*)data;
  return w;
}

// We need typecase functionality to implement polymorphic matrices properly.
