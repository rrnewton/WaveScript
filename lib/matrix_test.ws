
// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix-rowmajor.ws";



//mat = build_matrix(10,10, fun(i,j) intToFloat(i+j));

//mat0 = [[1,2], [3,4]];
//mat = fromList2d(mat0);

result = iterate (() in timer(30.0)) {
  state { first = true }


  // Can't yet statically compute this:
  mat0 = [[0.0+0.0i, 2.0+0.0i, 4.0+0.0i, 5.0+0.0i],
	  [3.0+0.0i, 4.0+0.0i, 5.0+0.0i, 0.0+0.0i - 2.0+0.0i],
	  [7.0+0.0i, 6.0+0.0i, 5.0+0.0i, 3.0+0.0i],
	  [4.0+0.0i, 6.0+0.0i, 5.0+0.0i, 7.0+0.0i]];
  mat = Matrix:fromList2d(mat0);

  if first then {
    m = Matrix:create(3, 4, 5.0);

    // These are some unit tests. 
   
    emit m;
    emit Matrix:trans(m);
    emit Matrix:mul(m,m);

    m2 = Matrix:create(5, 5, 3.0);
    
    simple = Matrix:fromList2d([[1.0, 2.0], [3.0 , 4.0]]);

    println("\n");

    println("Double Inverted, simple");
    print(m_invert( m_invert(simple)));
    println("\n");

    println("Multiply by inverse, simple:");
    print(Matrix:mul(simple, m_invert(simple)));
    println("\n");

    println("Multiply by inverse, pure:");
    print(pure_m_mult(simple, m_invert(simple)));
    println("\n");
    
    println("Double Inverted, bigger:");
    print(m_invert(m_invert(mat)));
    println("\n");

    println("Multiply by inverse:");
    print(m_mult(mat, m_invert(mat)));
    println("\n");

    //println("Numeric test:  " ++ );
  
    first := false;

  } else emit Matrix:create(0, 0, 0.0)  
}

BASE <- result;
//BASE <- CONST(nulltranspose())
//Matrix:noTrans
