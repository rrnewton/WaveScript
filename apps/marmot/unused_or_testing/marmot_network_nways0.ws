


/*============================================================*/
/* Node level, explicit broadcast */
S1 <- audio(1) 
S2 <- audio(2) 
S3 <- audio(3) 
S4 <- audio(4) 

P = prefilter(S1)

net <- BCAST <- P

H = combine(net,P)
D = sync(P,S1,S2,S3,S4)

Dat = sync(P,S1,S2,S3,S4)


BASE <- phase2()


/*============================================================*/
/* Network level, regiment style */

hints = map(prefilter,allnodes)

// A list of hints from our neighbors.
shared = table_gossip(hints)

// These line up one-for-one:
local_and_net = rzip(hints, shared)

fun p2(<hnt,ls>) { phase2(combine(ls,hnt)) }

BASE <- rfold(union, map(p2, shared))

//-----------------------------------
// For reference, here are the types:
hints :: Region <time,time>
shared :: Region (List <time,time>)
local_and_net :: Region (<<time,time>, List <time,time>>)

// Note: table_gossip is a library routine with this structure:
fun table_gossip(r) { rintegrate(timing_policy, gossip(r)) }



/*============================================================*/
/* All-to-all communication. */

forall N in Nodes {
  prefilter(N.audio1)

sync , N.audio2
  N.audio3
  N.audio4
    
}
