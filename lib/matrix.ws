
// A library of matrix routines.
// Author:  Lewis Girod & Ryan Newton 

// NOTE: rrn: I'm going to rewrite this at some point to use a single
// array (packed rows) representation for interoperability with GSL/Blas.

type Matrix t = Array (Array t);

// [2007.03.19] rrn: I'm going to clean things up gradually and move them into this namespace:
namespace Matrix {

// Interface:
get :: (Matrix t, Int, Int) -> t;
set :: (Matrix t, Int, Int, t) -> ();

 fun get(mat, row, col) (mat[row])[col];
 
 fun set(mat, row, col, val) {
   r = mat[row];
   r[col] := val;
   () // mutators should return nothing!
 };
 
  
};

// For now we just import immediately:
using Matrix;

// "Legacy" bindings:
m_get = Matrix:get;
m_set = Matrix:set;

// make:
matrix :: (Int, Int, t) -> (Matrix t);
fun matrix(rows, cols, init) {
  arr = Array:make(rows, Array:null);
  for i = 0 to rows-1 {
    arr[i] := Array:make(cols, init);
  };
  arr
}

// build:


fun m_zeroes(r,c) { matrix(r,c,gint(0)) }
fun m_ones(r,c) { matrix(r,c,gint(1)) }

fun m_rows(m) {
  m`Array:length
}

fun m_cols(m) {
  m[0]`Array:length
}

fun build_matrix(n,m,f) {
  Array:build(n, 
    fun(i) Array:build(m, 
      fun(j) f(i,j)))
}

// rrn: Pure version:
list_to_matrix :: List (List t) -> Array (Array t);
fun list_to_matrix(list) {
  len2 = list`head`List:length;
  Array:build(list`List:length,
    fun(i) Array:build(len2,
      fun(j) List:ref(List:ref(list,i), j)))
}

list_of_segs_to_matrix :: List (Sigseg t) -> Array (Array t);
fun list_of_segs_to_matrix(ls) {
  len = List:length(ls);
  arr = Array:make(len, Array:null);
  for i = 0 to len-1 {
    arr[i] := toArray(List:ref(ls, i))
  };
  arr
}

fun m_rowv_shared(m,i) {
  m[i]
}

fun m_rowm_shared(m,i) {
  Array:make(1,m[i]);
}

fun m_colv(m,i) {
  arr = Array:make(m`Array:length, gint(0));
  for j = 0 to m`Array:length-1 {
    arr[j] := m_get(m,j,i);
  };
  arr
}

fun m_colm(m,i) {
  arr = Array:make(m`Array:length, Array:null);
  for j = 0 to m`Array:length-1 {
    arr[j] := Array:make(1,m_get(m,j,i));
  };
  arr
}

fun m_map(f, m) {
  newm = Array:make(m_rows(m), Array:null);
  for i = 0 to (m_rows(m) -1) {
    newm[i] := amap(f, m[i])
  };
  newm
}

fun m_map_inplace(f, m) {
  for i = 0 to m_rows(m) {
    amap_inplace(f, m[i])
  };
  m
}

fun m_rowmap(f, m) {
  newm = Array:make(m_rows(m), Array:null);
  for i = 0 to (m_rows(m) - 1) {
    newm[i] := f(m[i])
  };
  newm
}

/* a very special rowmap that returns a vector */
fun m_rowmap_scalar(f, m) {
  newm = Array:make(m_rows(m), gint(0));
  for i = 0 to (m_rows(m) - 1) {
    newm[i] := f(m[i])
  };
  newm
}

/* rowmap that takes a function of row and index */
fun m_rowmap_index(f, m) {
  newm = Array:make(m_rows(m), Array:null);
  for i = 0 to m_rows(m) {
    newm[i] := f(m[i],i)
  };
  newm
}

fun m_pairmult(m1,m2) {
  nm = matrix(m_rows(m1),m_cols(m1),m_get(m1,0,0));
  for i = 0 to m_rows(m1) - 1 {
    for j = 0 to m_cols(m1) - 1 {
      m_set(nm,i,j,m_get(m1,i,j)*m_get(m2,i,j))
    }
  };
  nm
}

fun m_mult_scalar(m,s) {
  fun mm(x) {s*x};
  m_map(mm,m)
}

fun m_mult_scalar_inplace(m,s) {
  fun mm(x) {s*x};
  m_map_inplace(mm,m)
}


//================================================================================

m_mult :: (Array (Array #num), Array (Array #num)) -> Array (Array #num);
fun m_mult(m1,m2) {
  using Array;
  // TODO: could be more defensive here, check for null:
  m3 = matrix(m1`length, m2[0]`length, m_get(m1,0,0));
  for i = 0 to m1`length-1 {
    for j = 0 to m2[0]`length-1 {
      // need to know type :( .. what if not float?
      sum = Mutable:ref( gint(0) );
      for k = 0 to m2`length-1 {
	sum := sum + (m_get(m1,i,k) * m_get(m2,k,j));
      };
      m_set(m3,i,j,sum)
    }
  };
  m3 // Return.
}


// RRN: PURE version... experimenting with this.
pure_m_mult :: (Array (Array #num), Array (Array #num)) -> Array (Array #num);
fun pure_m_mult(m1,m2) {
  using Array;
  // TODO: could be more defensive here, check for null:
  build(    m1`length, fun (i)
  build( m2[0]`length, fun (j)
    foldRange(0, m2`length-1, gint(0), fun(sum, k) 
       sum + (m_get(m1,i,k) * m_get(m2,k,j)))))
}



m_trans :: Array (Array t) -> Array (Array t);
fun m_trans(m) {
  m2 = matrix(m[0]`Array:length, m`Array:length, m_get(m,0,0));
  for i = 0 to m`Array:length-1 {
    for j = 0 to m[0]`Array:length-1 {
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
/*   rank = m`Array:length; */
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

