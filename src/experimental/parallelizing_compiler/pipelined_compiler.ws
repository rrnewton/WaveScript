
// This is an experiment.  Could we make a pipelined compiler?

// This is the naive approach.  Just stream AST nodes.  I think this
// needs to get more sophisticated.  One idea would be to use locking,
// but lock every *level* of the AST rather than every node.
// Similarly, the tree could be streamed a level at a time...

include "stdlib.ws";

// Instead of a recursive data type, we use explicit indices to refer
// to other nodes in the tree.
type EPtr = Int;


// Let's split the tree up, so that we can stream AST nodes.
uniontype Expr = 
   ENum Float               | 
   EOp (Int * Int )         | // Just PLUS for now.
   ELam (Int)               | // Contains just a body
   EVar (Int)               | // The location of the binder.
   EApp (Int * Int)         |
   EEndToken (); // This shouldn't really be in the expression grammar

uniontype BinOp = PlusOp() | MultOp();


// Should make this a hash.
type AST = List (EPtr * Expr);

//ast0 = [ENum(90)]
//ast1 = [EOp(PlusOp(()), 34, 35)]


// A simple pass, handle a single clause in isolation.
fun pass1(strm) {
  smap(fun ((ind,exp))
    (ind, case exp {
      ENum(n): ENum(n+1)
      _ : exp
    }),
  strm)
}

// Next, something more complex.
// Let's reassemble all the lambdas and count varrefs to their bound var.
fun pass2(strm) {
  iterate (ind, exp) in strm {
    state {
      // These are lambdas waiting to have their arguments in place:
      waiting = HashTable:make(10);
      // This is the table where everything accumulates:
      ast = HashTable:make(100);
    }
    using HashTable;
    fun checkit(pos) {
      println("  checkin "++pos);
      if contains(ast,pos) then {
        case get(ast,pos) {
	  EEndToken(_): true
	  ENum(_)     : true
	  EVar(_)     : true
	  ELam(a)    : checkit(a)
          EOp(p) : { let (a,b) = p; checkit(a) && checkit(b) }
	  EApp(p): { let (a,b) = p; checkit(a) && checkit(b) }
	}
      } else false
    };
    case exp {
      ELam(bod): {
        println("  got lam");
        println("CHECKED: "++checkit(bod));
/*         go = Mutable:ref(true); */
/*         ptr = Mutable:ref(bod); */
/*         while go { */
/* 	  println("Checking for "++ptr++" in our ast..."); */
/* 	  if contains(ast, ptr) then { */
/* 	  } else { */
/* 	  } */
/* 	} */

      }
      _ : set_BANG(ast,ind,exp) 
    }
  }
}


ast = iterate _ in timer(3.0) {
  emit (1, EOp((1, 2)));
  emit (2, ENum(99));
  emit (3, ENum(101));
  emit (4, ELam(1));
  emit (-1, EEndToken(()));
}

/*   emit [ENum(90)]; */
/*   //  emit [EOp((PlusOp(()), 34, 35))]; */
/*   emit [EOp((34, 35))];
 */

BASE <- pass2 $ pass1 $ ast;

