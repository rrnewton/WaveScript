

// A library of matrix routines.
// Author:  Lewis Girod & Ryan Newton 


fun matrix(rows, cols, init) {
  arr = makeArray(rows, nullarr);
  for i = 0 to rows-1 {
    arr[i] := makeArray(cols, init);
  };
  arr
}

fun m_get(mat, row, col) {
  r = mat[row];
  r[col]
}

fun m_set(mat, row, col, val) {
  r = mat[row];
  r[col] := val;
  {}; // mutators should return nothing!
}

fun m_mult(m1,m2) {
  // TODO: could be more defensive here, check for nullarr:
  m3 = matrix(m1.length, m2[0].length, m_get(m1,0,0));
  for i = 0 to m1.length-1 {
    for j = 0 to m2[0].length-1 {
      // need to know type :( .. what if not float?
      sum = 0.0;
      for k = 0 to m2.length-1 {
	sum := sum +. (m_get(m1,i,k) *. m_get(m2,k,j));
      };
      m_set(m3,i,j,sum)
    }
  };

  m3
}

fun m_trans(m) {
  m2 = matrix(m[0].length, m.length, m_get(m,0,0));
  for i = 0 to m.length-1 {
    for j = 0 to m[0].length-1 {
      m_set(m2, j, i, m_get(m,i,j))
    }
  };
  m2
}




// This one is implemented directly in scheme.

fun m_invert(m) {
  det = m_determ(m);
  rank = mat.length;
  if det == 0 
  then matrix(0,0, (m[0])[0])
  eles matrix(0,0, (m[0])[0])
}


/* (define (matrix:inverse matrix) */
/*   (let* ((mat (matrix->lists matrix)) */
/*   	   (det (determinant mat)) */
/* 	   (rank (length mat))) */
/*     (and (not (zero? det)) */
/* 	 (do ((i rank (+ -1 i)) */
/* 	      (inv '() (cons */
/* 			(do ((j rank (+ -1 j)) */
/* 			     (row '() */
/* 				  (cons (/ (matrix:cofactor mat j i) det) row))) */
/* 			    ((<= j 0) row)) */
/* 			inv))) */
/* 	     ((<= i 0) */
/* 	      (coerce-like-arg inv matrix)))))) */



BASE <- iterate (x in audio(0,1000,0)) {
  state { first = True }
  
  if first then {
    m = matrix(3, 4, 5.0);
    
    // These are some unit tests.
    
    emit m;
    emit m_trans(m);
    emit m_mult(m,m);

  } else emit matrix(0, 0, 0.0);
}
