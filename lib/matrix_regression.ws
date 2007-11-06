
// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix-rowmajor.ws";



//mat = build_matrix(10,10, fun(i,j) intToFloat(i+j));

//mat0 = [[1,2], [3,4]];
//mat = fromList2d(mat0);

result = iterate (() in timer(30.0)) {
  state { first = true }

  if first then {

    // Can't yet statically compute this:
    mat0 = [[0.0+0.0i, 2.0+0.0i, 4.0+0.0i, 5.0+0.0i],
	    [3.0+0.0i, 4.0+0.0i, 5.0+0.0i, 0.0+0.0i - 2.0+0.0i],
	    [7.0+0.0i, 6.0+0.0i, 5.0+0.0i, 3.0+0.0i]];
    mat = Matrix:fromList2d(mat0);

    m = Matrix:create(3, 4, 5.0);

    // These are some unit tests. 
   
    println("Dimensions are 4,3? "++mat`Matrix.dims);
    println(show2(mat));
    println();

    println("Dimensions are 3,4? "++m`Matrix.dims);
    println(show2(m));
    println(show2(Matrix:trans(m)));
    println(show2(Matrix:mul(m,Matrix:trans(m))));
    println();

    m2 = Matrix:create(5, 5, 3.0);
    
    simple = Matrix:fromList2d([[1.0, 2.0], [3.0 , 4.0]]);

    println("Dimensions are 2,2? "++simple`Matrix.dims);
    println(show2(simple));
    println(show2(Matrix:trans(simple)));
    println(show2(Matrix:mul(simple,simple)));
    println();

    first := false;

  } 
}

BASE <- result;
