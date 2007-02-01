

// A library of matrix routines.
// Author:  Lewis Girod & Ryan Newton 

// NOTE: In the future we will have type aliases (shorthands) like this:
// type Matrix t = Array (Array t);

matrix :: (Int, Int, t) -> Array (Array t);
fun matrix(rows, cols, init) {
  arr = makeArray(rows, nullarr);
  for i = 0 to rows-1 {
    arr[i] := makeArray(cols, init);
  };
  arr
}


fun m_rows(m) {
  m.length
}

fun m_cols(m) {
  m[0].length
}


m_get :: (Array (Array t), Int, Int) -> t;
fun m_get(mat, row, col) (mat[row])[col]

m_set :: (Array (Array t), Int, Int, t) -> ();
fun m_set(mat, row, col, val) {
  r = mat[row];
  r[col] := val;
  {}; // mutators should return nothing!
}


// again need the init value for type check... 
fun list_to_matrix(list) {

  rows = listLength(list);
  cols = listLength(list.head);
  init = list.head.head;

  arr = makeArray(rows, nullarr);

  for i = 0 to rows-1 {
    row = listRef(list, i);
    arr[i] := makeArray(cols, init);
    for j = 0 to cols-1 {
      m_set(arr,i,j,listRef(row, j));
    }
  }
  arr
}


fun list_of_segs_to_matrix(ls) {
  len = listLength(ls);
  arr = makeArray(len, nullarr);
  for i = 0 to len-1 {
    arr[i] := to_array(listRef(ls, i))
  }
  arr
}


m_mult :: (Array (Array #num), Array (Array #num)) -> Array (Array #num);
fun m_mult(m1,m2) {
  // TODO: could be more defensive here, check for nullarr:
  m3 = matrix(m1.length, m2[0].length, m_get(m1,0,0));
  for i = 0 to m1.length-1 {
    for j = 0 to m2[0].length-1 {
      // need to know type :( .. what if not float?
      sum = gint(0);
      for k = 0 to m2.length-1 {
	sum := sum + (m_get(m1,i,k) * m_get(m2,k,j));
      };
      m_set(m3,i,j,sum)
    }
  };
  m3 // Return.
}

m_trans :: Array (Array t) -> Array (Array t);
fun m_trans(m) {
  m2 = matrix(m[0].length, m.length, m_get(m,0,0));
  for i = 0 to m.length-1 {
    for j = 0 to m[0].length-1 {
      m_set(m2, j, i, m_get(m,i,j))
    }
  };
  m2
}

/* (define (matrix:cofactor matrix i j) */
/*   (define mat (matrix->lists matrix)) */
/*   (define (butnth n lst) */
/*     (if (<= n 1) (cdr lst) (cons (car lst) (butnth (+ -1 n) (cdr lst))))) */
/*   (define (minor matrix i j) */
/*     (map (lambda (x) (butnth j x)) (butnth i mat))) */
/*   (coerce-like-arg */
/*    (* (if (odd? (+ i j)) -1 1) (determinant (minor mat i j))) */
/*    matrix)) */

/* (define (determinant matrix) */
/*   (define mat (matrix->lists matrix)) */
/*   (let ((n (length mat))) */
/*     (if (eqv? 1 n) (caar mat) */
/* 	(do ((j n (+ -1 j)) */
/* 	     (ans 0 (+ ans (* (list-ref (car mat) (+ -1 j)) */
/* 			      (matrix:cofactor mat 1 j))))) */
/* 	    ((<= j 0) ans))))) */

/* fun m_determ(m) { */
/*   333 */
/* } */
/* fun m_invert(m) { */
/*   det = m_determ(m); */
/*   rank = m.length; */
/*   if det == 0 */
/*   then matrix(0,0, (m[0])[0]) // Should return an option type. */
/*   else  */
/*     wserror("sigh") */
/*       /\* */
/*   out = matrix(...); */
/*   for i = rank downto 0 {  */
/*     for j = rank downto 0 {  */
/*       (out[])[] := cofactor(m, j, i); */
/*     } */
/*   } */
/*       *\/ */
/* } */

/* // I should do inverse directly in scheme. */
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


/*
BASE <- iterate (x in audio(0,1000,0)) {
  state { first = true }
  
  if first then {
    m = matrix(3, 4, 5.0);
    
    // These are some unit tests.
    
    emit m;
    emit m_trans(m);
    emit m_mult(m,m);

    m2 = matrix(5, 5, 3.0);

    

    first := false;
  } else emit matrix(0, 0, 0.0);
}
*/
