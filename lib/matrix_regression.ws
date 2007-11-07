
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
   
    println("Dimensions are 3,4? "++mat`Matrix:dims);
    println(Matrix:show2(mat));
    println("");

    println("Dimensions are 3,4? "++m`Matrix:dims);
    println(Matrix:show2(m));
    println(Matrix:show2(Matrix:trans(m)));
    println(Matrix:show2(Matrix:mul(m,Matrix:trans(m))));
    println("");

    Matrix:set(m,1,1,10);
    println(Matrix:show2(m));
    println(Matrix:show2(Matrix:trans(m)));
    println(Matrix:show2(Matrix:mul(m,Matrix:trans(m))));
    println("");

    simple = Matrix:fromList2d([[1.0, 2.0], [3.0 , 4.0]]);

    println("Dimensions are 2,2? "++simple`Matrix:dims);
    println(Matrix:show2(simple));
    println(Matrix:show2(Matrix:trans(simple)));
    println(Matrix:show2(Matrix:mul(simple,Matrix:trans(simple))));
    println("");

    first := false;

    println("Round tripping thru 1 d array");
    arr = Matrix:toArray(simple);
    println(Matrix:show2(Matrix:fromArray(arr,2)));

/*
    println("Round tripping thru 2 d array");
    arr2 = Matrix:toArray2d(simple);
    println(Matrix:show2(Matrix:fromArray2d(arr2)));
*/

    println("first col is "++Matrix:col(simple,0));
    println("first row is "++Matrix:row(simple,0));

    println("do a foreach:");
    Matrix:foreachi((fun(i,j,v) println("i="++i++" j="++j++" v="++v)),simple);

  } 
}

BASE <- result;
