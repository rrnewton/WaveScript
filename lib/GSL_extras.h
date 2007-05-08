

#ifdef __cplusplus 
extern "C" {
#endif


void* makeNullPerm(int);

void* makeMatrixWrapper(int x, int y, void* data);

void* gsl_matrix_data       (gsl_matrix*);
void* gsl_matrix_float_data (gsl_matrix*);

size_t gsl_matrix_size1      (gsl_matrix* mat);
size_t gsl_matrix_size2      (gsl_matrix* mat);
size_t gsl_matrix_float_size1(gsl_matrix_float* mat);
size_t gsl_matrix_float_size2(gsl_matrix_float* mat);
size_t gsl_matrix_complex_size1(gsl_matrix_complex* mat);
size_t gsl_matrix_complex_size2(gsl_matrix_complex* mat);

#ifdef __cplusplus 
}
#endif


