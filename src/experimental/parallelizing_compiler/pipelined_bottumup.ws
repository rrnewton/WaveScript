
// This is an experiment.  Could we make a pipelined compiler?

// This version streams nodes from the bottom of the tree up, one
// "level" (tree depth) at a time.

//include "stdlib.ws";
fun smap(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}

fun snd((a,b)) b

// Instead of a recursive data type, we use explicit indices to refer
// to other nodes in the tree.
type EPtr = Int;


// Let's split the tree up, so that we can stream AST nodes.
uniontype Expr ptr = 
   ENum Float               | 
   EOp (ptr * ptr)          | // Just PLUS for now.
   ELam (ptr)               | // Contains just a body
   EVar (Int)               | // The location of the binder.
   EApp (ptr * ptr)         |

   EEndLevel Int            | // This shouldn't really be in the expression grammar
   EEndExpr ()              ; // This shouldn't really be in the expression grammar

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

// Join App nodes with children.

// Join all nodes with children.  Output each level independently
// (even though it will have already been output as the child of the
// previous level.
//join_all :: Stream (Int * Expr ind) -> Stream (Int * Expr (Expr ind));
fun join_all(strm) {
  iterate (ind, exp) in strm {
    state {
      // Everyone from last lvl (below):
      lastlvl = []; //HashTable:make(10); 
      thislvl = [];//HashTable:make(10); 
    }
    //using HashTable;
    //set_BANG(thislvl, ind, exp);
    thislvl := List:assoc_update(thislvl, ind, exp);

    case exp {
/*       ENum (f) :    {} */
/*       ELam (ptr) :  {} */
/*       EVar (ptr) :  {} */
/*       EOp (pair) :  { let(x,y)=pair; } */
/*       EApp (pair) : { let(x,y)=pair; }          */
/*       EEndExpr(_) : {}  */
      EEndLevel(n): {
        // When we're done with the current level, we can join it with the previous level.
	// NEED HASHTABLE:FOREACH
        List:map(fun((ind,exp)) 
	    case exp {
      	      ELam (ptr):  emit (ind, ELam(List:assoc(lastlvl, ptr)`head`snd))
	      EVar (n)  :  emit (ind, exp)
/* 	      EOp (pair) :  { let(x,y)=pair; } */
/* 	      EApp (pair) : { let(x,y)=pair; }          */
/* 	      EEndLevel(n): {} */
//              _ : emit exp
	      
	    }, thislvl);
	//lastlvl := thislvl;
	thislvl := [];
	()
      }
      _ : {}
    }

    // Is somebody waiting to connect to us?
    //if contains(ind,lastlvl)
    
    
  }  
}


/* // Next, something more complex. */
/* // Let's reassemble all the lambdas and count varrefs to their bound var. */
/* fun pass2(strm) { */
/*   iterate (ind, exp) in strm { */
/*     state { */
/*       // These are lambdas waiting to have their arguments in place: */
/*       waiting = HashTable:make(10); */
/*       // This is the table where everything accumulates: */
/*       ast = HashTable:make(100); */
/*     } */
/*     using HashTable; */
/*     fun checkit(pos) { */
/*       println("  checkin "++pos); */
/*       if contains(ast,pos) then { */
/*         case get(ast,pos) { */
/* 	  EEndToken(_): true */
/* 	  ENum(_)     : true */
/* 	  EVar(_)     : true */
/* 	  ELam(a)    : checkit(a) */
/*           EOp(p) : { let (a,b) = p; checkit(a) && checkit(b) } */
/* 	  EApp(p): { let (a,b) = p; checkit(a) && checkit(b) } */
/* 	} */
/*       } else false */
/*     }; */
/*     case exp { */
/*       ELam(bod): { */
/*         println("  got lam"); */
/*         println("CHECKED: "++checkit(bod)); */
/* /\*         go = Mutable:ref(true); *\/ */
/* /\*         ptr = Mutable:ref(bod); *\/ */
/* /\*         while go { *\/ */
/* /\* 	  println("Checking for "++ptr++" in our ast..."); *\/ */
/* /\* 	  if contains(ast, ptr) then { *\/ */
/* /\* 	  } else { *\/ */
/* /\* 	  } *\/ */
/* /\* 	} *\/ */

/*       } */
/*       _ : set_BANG(ast,ind,exp)  */
/*     } */
/*   } */
/* } */


ast = iterate _ in timer(3.0) {
  emit (3, ENum(99));
  emit (3, ENum(101));
  emit (0, EEndLevel(3));

  emit (2, ELam(1));
  emit (0, EEndLevel(2));

  emit (1, EOp((1, 2)));
  emit (0, EEndLevel(1));

  emit (-1, EEndExpr(()));
}

joined = iterate _ in timer(3.0) {
  emit (ELam(ELam("foo")));
}

/*   emit [ENum(90)]; */
/*   //  emit [EOp((PlusOp(()), 34, 35))]; */
/*   emit [EOp((34, 35))];
 */

BASE <- join_all $ pass1 $ ast;
//BASE <- ast
//BASE <- joined

