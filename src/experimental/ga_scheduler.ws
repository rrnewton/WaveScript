

// I'm thinking about writing a task-placement algorithm in the language.
// After all the problem is computationally intensive and has abundant parallelism.

include "stdlib.ws";
include "matrix.ws";

type Graph = Matrix Int;

// A chromosome is simply an assignment of task to processors for now.
type Chromosome = Array Int; //Int16;

popsize = 100;
total_generations = 10;

// Mutation happens to an individual one in N generations:
mutaterate = 10;
crossrate  = 10;

//================================================================================

nullchrom = Array:null;

// Computation power of processors:
processors = List:toArray$ 
   [1, 1, 1];

// Communication cost of network links (delays)
commnetwork = Matrix:fromList2d$
  [[0, 3, 3],
   [3, 0, 3],
   [3, 3, 0]];

// Computational cost of each task.
taskweights = List:toArray$ 
  [20, 30, 40, 50];

// A pipeline topology, weights represent communication quantity.
taskgraph = Matrix:fromList2d$
  [[0, 1, 0, 0],
   [0, 0, 1, 0],
   [0, 0, 0, 1],
   [0, 0, 0, 0]];

numprocs   = Array:length(processors);
numtasks   = Array:length(taskweights);
sumweights = Array:fold1((+), taskweights);

sumcomms   = Matrix:fold((+), 0, taskgraph);

//================================================================================
// Implement the GA itself.

fun fitness(chrom) {
  using Array; using Mutable;
  // Sum the weights across.
  bins = make(length(processors), 0);
  for i = 0 to taskweights.length - 1 {
    ind = chrom[i];
    bins[ind] := bins[ind] + taskweights[i];
  };
  procbalance = sumweights - fold1(max, bins);

  // Sum the cost of each edge in the graph
  commcost = ref(0);
  Matrix:foreachi(fun(i,j, weight)  // Could use foldi if it existed.
    {
      commcost += weight * Matrix:get(commnetwork, chrom[i], chrom[j]);
    }, taskgraph);

  //println(" Total commcost is "++commcost++" for "++chrom);

  // Return a combination of these measures.
  procbalance + commcost
}

crossover :: (Chromosome, Chromosome) -> (Chromosome * Chromosome);
fun crossover(chrom1, chrom2) {
  using Array;
  // Single point crossover:
  if (0 == crossrate) then {
    len = chrom1.length;
    ind = randomI(len);
    new1 = makeUNSAFE(len);
    new2 = makeUNSAFE(len);
    blit(new1,0, chrom1,0, ind);
    blit(new2,0, chrom2,0, ind);
    blit(new1,ind, chrom2,ind, len-ind);
    blit(new2,ind, chrom1,ind, len-ind);
    (new1,new2)
  } else (chrom1, chrom2)
}

fun cross_all(pop) {
  newpop = Array:make(popsize, nullchrom);
  for i = 0 to (popsize/2)-1 {
    let (a,b) = crossover(pop[i*2], pop[(i*2) +1]);
    newpop[i*2]   := a;
    newpop[(i*2)+1] := b;
  };
  newpop  
}

fun mutate(chrom) {
  if randomI(mutaterate) == 0 then {
    chrom2 = Array:copy(chrom);
    // Move a random task to a random processor:
    chrom2[randomI(Array:length(chrom2))]
      := randomI(Array:length(processors));
    chrom2
  } else chrom;
}

fun mutate_all(pop) {
  Array:map(mutate, pop);
}

// Centralized selection.
fun selection(taggedpop) {
  using Array;
  //fits = build(popsize, fun(i) fitness(pop[i]));
  //map(fun(c) println("Fitness "++fitness(c)++" "++c), pop);

  newpop = Array:make(popsize, nullchrom);
  // No elitism currently.
  for i = 0 to popsize-1 {
    xi = randomI(popsize);
    yi = randomI(popsize);
    let (x,fitx) = taggedpop[xi];
    let (y,fity) = taggedpop[yi];
    if fitx > fity   //fits[xi] > fits[yi]
    then newpop[i] := x //pop[xi]
    else newpop[i] := y //pop[yi]    
  };
  newpop
}

// Takes a whole population in an Array and produces the next generation.
fun advance_population(pop) {
  using Array;
  pop0 = map(fun(x) (x, fitness(x)), pop);
  pop1 = selection(pop0);
  pop2 = map(mutate, pop1);
  pop3 = cross_all(pop2);
  /*  println("Fitness  : "++pop0);
  println("Selection: "++pop1);
  println("Mutation : "++pop2);
  println("Crossover: "++pop3);*/

  pop3
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
// Setup the stream dataflow graph that will run the GA

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
	   //println(" Pop: "++pop);
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

fun tagWith(fn, strm) smap(fun(x) (x,fn(x)), strm);

fun pipelined(initpop) {
  batches = smap(toArray, window(initpop, popsize));
  using Curry;
  smap(cross_all)  $ 
  smap(mutate_all) $ 
  smap(selection)  $ 
    // window
    // tagWith(fitness,x)
    // dewindow
    //smap(fun(x) )$ a
    batches;
}

fun pickwinner(strm) {
  smap(fun(arr) {
    println("Extracting winner from final population.");
    Array:fold(fun((best,bfit), new) {
        fit = fitness(new);
        if fit > bfit
	then (new,fit)
	else (best,bfit)
      }, 
      (arr[0], fitness(arr[0])),
      arr)
  }, strm)
}

seedpop :: Stream Chromosome;
seedpop = iterate i in FINITE(popsize) {
  emit Array:build(numtasks, fun(_) randomI(numprocs));
}

main = pickwinner$ mainloop(seedpop)
//main = simple_mixer(10, COUNTUP(0));

// This is non-recursive.
//main = oneiter(seedpop);
//main = pipelined(seedpop);

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
