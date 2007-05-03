


//#include <gsl/gsl_linalg.h>
//#include <gsl/gsl_matrix.h>

#include "gsl/gsl_linalg.h"
#include "gsl/gsl_matrix.h"

// This makes an identity permutation
void* makeNullPerm(int size) {
  gsl_permutation* perm = gsl_permutation_calloc(size);
  return perm;
}
