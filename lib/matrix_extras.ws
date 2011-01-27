
/* 

 These are some extra array utils

 author: Ryan Newton

*/

// Doesn't commit to a matrix implementation.
//include "matrix.ws";

namespace Matrix {

  fun fold(f,zer,mat) {
    acc = Mutable:ref(zer);
    Matrix:foreachi(fun(i,j,elm) acc := f(acc, elm), mat);
    acc
  }

  fun foldi(f,zer,mat) {
    acc = Mutable:ref(zer);
    Matrix:foreachi(fun(i,j,elm) acc := f(i,j, acc, elm), mat);
    acc
  }

  fun transpose(mat) {
    let (row,col) = dims(mat);
    build(col, row, fun(c,r) get(mat,r,c))
  }
}
