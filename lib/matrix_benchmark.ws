
/*
*  benchmark results.  rowmajor: 6270
*                      original: 11614
*                      rowmajor hand-opt indexing: 3365
*
*/

// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix-rowmajor.ws";
//include "matrix.ws";


 // Matrix multiplication.
 fun mul_rm_opt_index(m1,m2) {
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
      for k = 0 to r2-1 {
	sum[tmps] := sum[tmps] + arr1[tmpi] * arr2[tmpj];
        tmpi := tmpi + 1;
        tmpj := tmpj + c2;
        tmps := (tmps + 1); if (tmps = 8) then tmps := 0;
      };
      arr_o[tmpiout+j] := Array:fold((+),gint(0),sum);
    }
  };
  m3 // Return.
 }


result = iterate (() in timer(30.0)) {
  state { first = true }

  if first then {
    first := false;

    m = Matrix:build(1000,1000,fun (i,j) intToDouble(i*j));

    start1 = clock();
    //m2 = Matrix:mul(m,m);
    m2 = mul_rm_opt_unroll(m,m);
    start2 = clock();

    println("elapsed time: "++start2-start1);

    emit(());
  } 
}

BASE <- result;
