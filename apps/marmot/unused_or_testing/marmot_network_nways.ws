

// For this example, we assume that bcast also sends to *myself*.

/*============================================================*/
/* Node level, explicit broadcast */
S1 <- audio(1) 
S2 <- audio(2) 
S3 <- audio(3) 
S4 <- audio(4) 

netH <- BCAST <- prefilter(S1)

D = sync(vote(netH), S1,S2,S3,S4)

BASE <- phase2(D)


/*============================================================*/
/* Network level, regiment style, still using local broadcast */

hints :: Region <time,time>
hints = rmap(prefilter,ALLNODES)

// A list of hints from our neighbors for this time window.
shared :: Region (List <time,time>)
shared = group_epoch(gossip(hints))

BASE <- rfold(union, rmap(p2, shared))

fun p2 (nd, ls) {
  D = sync(vote(ls), 
	   nd.audio1, nd.audio2, 
	   nd.audio3, nd.audio4);
  return phase2(D)
}

// Note: group_epoch is a library routine with this structure:
fun group_epoch(r) { rintegrate(timing_policy, r) }


/*============================================================*/
/* All-to-all communication. */

hints = rmap(prefilter,ALLNODES)

// There's a single conclusion for this time range.
conclusion = smap(vote, unionall(P))

fun p2 (nd, ls) {
  D = sync(conclusion, 
	   nd.audio1, nd.audio2, 
	   nd.audio3, nd.audio4);
  return phase2(D)
}

BASE <- rfold(union, rmap(p2, ALLNODES));



flood_dist(300m,hints)


/*============================================================*/
/* With nested regions, distance based grouping */

hints :: Region <time,time>
hints = rmap(prefilter,ALLNODES)

// A list of hints from our neighbors for this time window.
shared :: Region (List <time,time>)
shared = rmap(rfold(append,[]), 
	      geohood(300m, hints))

BASE <- rfold(union, rmap(p2, shared))

fun p2 (nd, ls) {
  D = sync(vote(ls), 
	   nd.audio1, nd.audio2, 
	   nd.audio3, nd.audio4);
  return phase2(D)
}

// Note: group_epoch is a library routine with this structure:
fun group_epoch(r) { rintegrate(timing_policy, r) }







================================================================================
SCRAP:

P = rmap(fun (N) prefilter(N.audio1), ALLNODES)

P = forall N in Nodes {
  prefilter(N.audio1)    
}


results = forall N in Nodes {
  D = sync(conclusion,
	   N.audio1, N.audio2
	   N.audio3, N.audio4)
  phase2(D)
}


BASE <- results;

//-----------------------------------
// For reference, here are the types:



local_and_net :: Region (<<time,time>, List <time,time>>)

shared = table_gossip(hints)
// Note: table_gossip is a library routine with this structure:
fun table_gossip(r) { rintegrate(timing_policy, gossip(r)) }
