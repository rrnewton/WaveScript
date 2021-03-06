
// TESTS *OLD* matrix file.

// Matrix.ws needs stdlib.ws.

include "stdlib.ws";
include "matrix_old.ws";



//mat = build_matrix(10,10, fun(i,j) intToFloat(i+j));

//mat0 = [[1,2], [3,4]];
//mat = list_to_matrix(mat0);

result = iterate (() in timer(30.0)) {
  state { first = true }

  // Can't yet statically compute this:
  mat0 = [[0.0+0.0i, 2.0+0.0i, 4.0+0.0i, 5.0+0.0i],
	  [3.0+0.0i, 4.0+0.0i, 5.0+0.0i, 0.0+0.0i - 2.0+0.0i],
	  [7.0+0.0i, 6.0+0.0i, 5.0+0.0i, 3.0+0.0i],
	  [4.0+0.0i, 6.0+0.0i, 5.0+0.0i, 7.0+0.0i]];
  mat = list_to_matrix(mat0);

  if first then {
    m = matrix(3, 4, 5.0);

    // These are some unit tests. 
   
    emit m;
    emit m_trans(m);
    emit m_mult(m,m);

    m2 = matrix(5, 5, 3.0);
    
    simple = list_to_matrix([[1.0, 2.0], [3.0 , 4.0]]);

    // Removing matrix inversion as a primitive:
/*
    println("\nDouble Inverted, simple");
    println(m_invert( m_invert(simple)));
    println("Multiply by inverse, simple:");
    println(m_mult(simple, m_invert(simple)));
    println("Multiply by inverse, pure: ");
    println(pure_m_mult(simple, m_invert(simple)));
    println("Double Inverted, bigger:");
    println(m_invert(m_invert(mat)));
    println("Multiply by inverse:");
    println(m_mult(mat, m_invert(mat)));
*/ 

    //println("Numeric test:  " ++ );
  
    first := false;

  } else emit matrix(0, 0, 0.0)  
}

BASE <- result;
//BASE <- CONST(nulltranspose())
//Matrix:noTrans
