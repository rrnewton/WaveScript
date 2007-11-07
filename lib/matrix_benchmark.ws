
/*
*  benchmark results.
*   c test:                12300
*   original:              11614
*   rowmajor:               6270
*   rowmajor with list:     6323
*   rowmajor lift mults:    4667
*   rowmajor index by adds: 3365
*   rowmajor unroll:        1956
*
*/

// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix-rowmajor.ws";
//include "matrix-rowmajor-spec.ws";
//include "matrix.ws";

////////////////////////
//
// *** NOTE THE FOLLOWING OPTS ONLY WORK WITH THE MATRIX-ROWMAJOR.WS
//     VERSION.  ALL OTHERS HAVE AN INCOMPATIBLE DATA REP
//
///////////////////////

 // Matrix multiplication.
 fun mul_rm_opt_index(m1,m2) {
  using Array;
  let (r1,c1,arr1) = m1;
  let (r2,c2,arr2) = m2;

  m3 = Matrix:create(r1, c2, Matrix:get(m1,0,0));
  let (ro,co,arr_o) = m3;

  for i = 0 to r1-1 {
    tmpiout = i*c2;
    tmpi = i*c1;
    for j = 0 to c2-1 {
      sum = Mutable:ref( gint(0) );
      for k = 0 to r2-1 {
	sum := sum + arr1[tmpi+k] * arr2[k*c2+j];
      };
      arr_o[tmpiout+j] := sum;
    }
  };
  m3 // Return.
 }


 // Matrix multiplication.
 fun mul_rm_opt_index2(m1,m2) {
  using Array;
  let (r1,c1,arr1) = m1;
  let (r2,c2,arr2) = m2;

  m3 = Matrix:create(r1, c2, Matrix:get(m1,0,0));
  let (ro,co,arr_o) = m3;

  for i = 0 to r1-1 {
    tmpiout = i*c2;
    for j = 0 to c2-1 {
      sum = Mutable:ref( gint(0) );
      tmpi = Mutable:ref(i*c1);
      tmpj = Mutable:ref(j);
      for k = 0 to r2-1 {
	sum := sum + arr1[tmpi] * arr2[tmpj];
        tmpi := tmpi + 1;
        tmpj := tmpj + c2;
      };
      arr_o[tmpiout+j] := sum;
    }
  };
  m3 // Return.
 }


 // Matrix multiplication.
 fun mul_rm_opt_unroll(m1,m2) {
  using Array;
  let (r1,c1,arr1) = m1;
  let (r2,c2,arr2) = m2;

  m3 = Matrix:create(r1, c2, Matrix:get(m1,0,0));
  let (ro,co,arr_o) = m3;

  sum = Array:make(8,gint(0));

  for i = 0 to r1-1 {
    tmpiout = i*c2;
    for j = 0 to c2-1 {
      for k = 0 to 7 { sum[k] := gint(0); };
      tmpi = Mutable:ref(i*c1);
      tmpj = Mutable:ref(j);
      tmps = Mutable:ref(0);
      fun unroll(index) {
        sum[index] := sum[index] + arr1[tmpi] * arr2[tmpj];
        tmpi := tmpi + 1;
        tmpj := tmpj + c2;
      };
      for k = 0 to (r2/8)-1 {
        unroll(0);
        unroll(1);
        unroll(2);
        unroll(3);
        unroll(4);
        unroll(5);
        unroll(6);
        unroll(7);
      };
      for k = 8*(r2/8) to r2-1 {
	unroll(0);
      };
      arr_o[tmpiout+j] := Array:fold((+),gint(0),sum);
    }
  };
  m3 // Return.
 }

 // Matrix multiplication.
 fun mul_rm_opt_unroll_generalized(m1,m2,unrollcount) {
  using Array;
  let (r1,c1,arr1) = m1;
  let (r2,c2,arr2) = m2;

  m3 = Matrix:create(r1, c2, Matrix:get(m1,0,0));
  let (ro,co,arr_o) = m3;

  sum = Array:make(unrollcount,gint(0));

  for i = 0 to r1-1 {
    tmpiout = i*c2;
    for j = 0 to c2-1 {
      for k = 0 to 7 { sum[k] := gint(0); };
      tmpi = Mutable:ref(i*c1);
      tmpj = Mutable:ref(j);
      tmps = Mutable:ref(0);
      fun unroll(index) {
        sum[index] := sum[index] + arr1[tmpi] * arr2[tmpj];
        tmpi := tmpi + 1;
        tmpj := tmpj + c2;
      };
      for k = 0 to (r2/unrollcount)-1 {
        for qq = 0 to unrollcount-1 { unroll(qq); }
      };
      for k = unrollcount*(r2/unrollcount) to r2-1 {
	unroll(0);
      };
      arr_o[tmpiout+j] := Array:fold((+),gint(0),sum);
    }
  };
  m3 // Return.
 }

////////////////////////
//
// *** END SPECIAL JUNK
//
///////////////////////


result = iterate (() in timer(30.0)) {
  state { first = true }

  if first then {
    first := false;

    start0 = clock();

    matsize = 500;

    m1 = Matrix:build(matsize,matsize,fun (i,j) intToDouble(i+j));
    m2 = Matrix:build(matsize,matsize,fun (i,j) intToDouble(i*j));

    start1 = clock();

    //m3 = Matrix:mul(m,m);
    //m3 = mul_rm_opt_index(m,m);
    //m3 = mul_rm_opt_index2(m,m);
    m3 = mul_rm_opt_unroll(m1,m2);

    start2 = clock();

    println("elapsed time: "++start2-start1);
    println("elapsed time including build: "++start2-start0);

    sum = Mutable:ref(gint(0));
    for i = 0 to matsize-1 {
      for j = 0 to matsize-1 {
        sum := sum + Matrix:get(m3,i,j);
    }};

    println("sum="++sum);

    emit(());
  } 
}

BASE <- result;
