

// I'm thinking about writing a task-placement algorithm in the language.
// After all the problem is computationally intensive and has abundant parallelism.

include "stdlib.ws";
include "matrix.ws";

type Graph = Matrix Int;


// A chromosome is simply an assignment of task to processors for now.
type Chromosome = Array Int16;

popsize = 5;
total_generations = 10;

//================================================================================








//================================================================================
// Implement the GA itself.

fun fitness(chrom) {
  99 + chrom[2]
}

crossover :: (Chromosome, Chromosome) -> Chromosome;
fun crossover(chrom1, chrom2) {
  chrom1
}

fun mutate(chrom) {
  chrom2 = Array:copy(chrom);
  chrom2[2] := 99;
  chrom2
}

// Takes a whole population in an Array and produces the next generation.
fun advance_population(pop) {
  using Array;
  pop1 = map(mutate, pop);
  pop1 
}


//================================================================================

// Playing around with wiring diagrams that mix things up.

fun uniformperm(n) {
  using Array;
  perm = build(n, fun(i) i);
  for i = 0 to n-2 {
    j = randomI(n-i-1) + 1;
    tmp = perm[i];
    perm[i] := perm[i+j];
    perm[i+j] := tmp;
  }
  perm
}

fun simple_mixer(size, strm) {
  //batched = window(strm, size);
  strms = List:toArray$ deinterleave(size, strm);
  // Now mix up the wires:
  perm = uniformperm(size);
  mixed = List:build(size, fun(i) strms[perm[i]]);
  //unionList(mixed);
  zipN(2*size, mixed);
}


//================================================================================
// Setup 

uniontype Tag t = Orig t | Recycle t | Output t ;
fun detag(t) {
 case t {
   Orig(x):    x
   Recycle(x): x
   Output(x):  x
 }
}

//mainloop :: (Stream Chromosome) -> (Stream Chromosome); // HUH?? why does this barf
fun mainloop(instrm) {
  batches = smap(toArray, window(instrm, popsize));
  tagged = smap(Orig, batches);
  out = feedbackloop(tagged,
   fun(loopback) {
     iterate x in loopback {
       state { generation = 0 }
       case x {
         Orig(c): {
	   print("  Bouncing to recycle...\n");
           emit Recycle(c);
	 }
         Recycle(pop): {
           generation += 1;
	   println(" Running generation "++generation);
	   pop2 = advance_population(pop);
	   if (generation >= total_generations)
	   then emit Output(pop2)
	   else emit Recycle(pop2);
	 }
         Output(c): {
           print("Looped through, ignored.\n");
	 }
       };
     }     
   });
  detagged = smap(detag, sfilter(fun(x) case x { Output(c): true _: false}, out));
  detagged 
}

// 
fun oneiter(initpop) {
  batches = smap(toArray, window(initpop, popsize));
  smap(advance_population, batches);
}



seedpop :: Stream Chromosome;
seedpop = iterate i in FINITE(popsize) {
  emit Array:make(5,(5::Int16));
}

//main = mainloop(seedpop)
main = simple_mixer(10, COUNTUP(0));

// This is non-recursive.
//main = oneiter(seedpop);

/*
main = iterate _ in timer(10) {  
  emit ();
}


seedpop

together = merge(win_seedpop, loopback)

fit = smap(fitness, dewindow(together))

wins = window(fit, 30)

loopback = smap(crossover_win, wins)

main = loopback


*/
