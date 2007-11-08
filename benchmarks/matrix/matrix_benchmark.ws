
/*
*  benchmark results.       O3      no O3
*   c test:                 1510
*   original:               2218    2606
*   rowmajor:               2220    2906
*   rowmajor with list:     6323
*   rowmajor lift mults:    4667
*   rowmajor index by adds: 3365
*   rowmajor unroll:        1956
*
*/

// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix.ws";
include "bench_common.ws";

result = iterate (() in timer(30.0)) {
  state { first = true }

  if first then {
    first := false;

    runtest(Matrix:mul);

    emit(());
  } 
}

BASE <- result;
