

/* 

This file defines various kernels with different allocation profiles.

They can then be combined in various topologies.

 */


// Containers, lists or arrays:
listcont = (| Make = fun (n) List:make(n,0) 
           )
arrcont  = (| Make = fun (n) Array:make(n,0)
           )

// Uh, oh, this actually causes an incorrect abuse of type shorthands
// when the compiler prints out types, interesting: [2009.01.21]
//type Container = (Make : Int -> ignored);
type Container t = (Make : Int -> t);

// Allocates a bunch of containers.
fun keep_random_send_random(size, nummake, numkeep)
 fun (container, S) {
  make = container.Make;
  iterate x in S {
    //state { arr = Array:make(numkeep, make(0)) }
    state { arr = Array:null }
    
    // This just aliases the first one allocated.
    if Array:length(arr) == 0
    then arr := Array:make(numkeep, make(size));

    // Store the input into a random space.
    arr[randomI(numkeep)] := x;
    for i = 1 to nummake {
      arr[randomI(numkeep)] := make(size);
    };
    emit arr[randomI(numkeep)];
  }
}

// Assembling kernels:
//==============================================================================

fun chain(N, container, construct) {
  make = container.Make;
  source = iterate _ in timer(100) { emit make(100) };
  chain = source;
  for i = 1 to N { 
    chain := construct(container, chain)
  };
  chain
}
