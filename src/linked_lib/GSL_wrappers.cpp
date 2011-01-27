
// This is sucked into the query file only if the functionality is needed.

typedef boost::shared_ptr< vector<wscomplex_t> > myarr_t;
typedef boost::shared_ptr<vector< myarr_t > > mymatrix_t;

   // This uses GSL:
   static mymatrix_t m_invert(mymatrix_t mat)
   {
     int rows = mat->size();
     int cols = (*mat)[0]->size();
     gsl_matrix_complex * m_in = gsl_matrix_complex_alloc(rows, cols);
     // Fill it up with our matrix.
     for(int r=0; r<rows; r++)
       for(int c=0; r<cols; c++) {
	 gsl_complex gcf;
	 wscomplex_t wcf = (*((*mat)[r]))[c];
	 GSL_SET_REAL(&gcf, (double)__real__ wcf);
	 GSL_SET_IMAG(&gcf, (double)__imag__ wcf);
	 gsl_matrix_complex_set(m_in, r, c, gcf);
       }

     gsl_permutation* perm = gsl_permutation_calloc(rows);
     gsl_matrix_complex * m_out = gsl_matrix_complex_alloc(rows, cols);

     // Run the inverse.
     gsl_linalg_complex_LU_invert(m_in, perm, m_out);

     // Allocate matrix for result:
     mymatrix_t result(new vector<myarr_t>(rows));
     for(int i=0; i<(int)rows; i++) {
       (*result)[i] = myarr_t(new vector<wscomplex_t>(cols));
     }

     // Read back out the result:
     for(int r=0; r<rows; r++)
       for(int c=0; r<cols; c++) {
	 gsl_complex gcf = gsl_matrix_complex_get(m_in, r, c);
	 wscomplex_t wcf = (float)GSL_REAL(gcf) + ((float)GSL_IMAG(gcf) * 1.0fi);
	 (*(*result)[r])[c] = wcf;
       }

     return result; 
   }
