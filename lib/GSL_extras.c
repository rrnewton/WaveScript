


//#include <gsl/gsl_linalg.h>
//#include <gsl/gsl_matrix.h>

#include "gsl/gsl_linalg.h"
#include "gsl/gsl_matrix.h"
#include "gsl/gsl_blas.h"

// This makes an identity permutation
void* makeNullPerm(int size) {
  gsl_permutation* perm = gsl_permutation_calloc(size);
  return perm;
}

// ENUM: No transpose
int makeCblasNoTrans() {
  return CblasNoTrans;
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


void* gsl_matrix_data        (gsl_matrix* mat)       { return mat->data; }
void* gsl_matrix_float_data  (gsl_matrix_float* mat) { return mat->data; }
void* gsl_matrix_complex_float_data  (gsl_matrix_complex_float* mat) { return mat->data; }

size_t gsl_matrix_size1      (gsl_matrix* mat)       { return mat->size1; }
size_t gsl_matrix_size2      (gsl_matrix* mat)       { return mat->size2; }
size_t gsl_matrix_float_size1(gsl_matrix_float* mat) { return mat->size1; }
size_t gsl_matrix_float_size2(gsl_matrix_float* mat) { return mat->size2; }
size_t gsl_matrix_complex_size1(gsl_matrix_complex* mat) { return mat->size1; }
size_t gsl_matrix_complex_size2(gsl_matrix_complex* mat) { return mat->size2; }
size_t gsl_matrix_complex_float_size1(gsl_matrix_complex_float* mat) { return mat->size1; }
size_t gsl_matrix_complex_float_size2(gsl_matrix_complex_float* mat) { return mat->size2; }

// This is horrible:
float get_complex_ptr_real(gsl_complex_float* c) {
  float* p = (float*)c;
  return p[0];
}
float get_complex_ptr_imag(gsl_complex_float* c) {
  float* p = (float*)c;
  return p[1];
}

gsl_complex_float cmplx_comm_temp;
gsl_complex_float* ws_gsl_matrix_complex_float_get(gsl_matrix_complex_float* mat, int i, int j) {
  cmplx_comm_temp = gsl_matrix_complex_float_get(mat,i,j);
  return &cmplx_comm_temp;
}
void* ws_gsl_matrix_complex_float_set(gsl_matrix_complex_float* mat, int i, int j, float re, float im) {
  gsl_complex_float c;
  float* p = (float*)(&c);
  p[0] = re;
  p[1] = im;
  gsl_matrix_complex_float_set(mat,i,j, c);
}

void* ws_gsl_matrix_complex_float_scale(gsl_matrix_complex_float* mat, float re, float im) {
  gsl_complex_float c;
  float* p = (float*)(&c);
  p[0] = re;
  p[1] = im;  
  gsl_matrix_complex_float_scale(mat, c);
}

void* ws_gsl_matrix_complex_float_add_constant(gsl_matrix_complex_float* mat, float re, float im) {
  gsl_complex_float c;
  float* p = (float*)(&c);
  p[0] = re;
  p[1] = im;  
  gsl_matrix_complex_float_add_constant(mat, c);
}
