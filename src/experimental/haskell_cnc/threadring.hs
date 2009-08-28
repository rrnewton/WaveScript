{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures, NamedFieldPuns, RecordWildCards #-}

-- Memoize bad!
-- #define MEMOIZE 
#include "stub.h"


-- This simple microbenchmark is drawn from the "Great Language Shootout".
-- It passes token(s) around a ring.

-- This version uses a separate tag collection to represent each actor.

threadring hops agents 1 =
       do 
	  answer :: ItemCol Int Int <- newItemCol() 

          first:resttags <- mapM (\i -> do x <- newTagCol(); return (i,x))
			         [1..agents]

          foldM (\ (i,last) (j,next) ->
		 do prescribe last
		     (\n -> if n == 0 
		            then put answer 0 i
		            else call next (n-1))
		    return (j,next))
		first (resttags++[first])

	  initialize $ 
	     do call (snd$ first) hops

 	  finalize $ get answer 0

main =   
  do ls <- System.getArgs 
     v <- runGraph $ 
       case Prelude.map read ls of 
        []      -> threadring 17 503 1
	[h]     -> threadring h  503 1
	[h,a]   -> threadring h  a   1
	[h,a,t] -> threadring h  a   t
	  
     putStrLn (show v)

{- 
NOTES:

[2009.08.12] 
  Trying 5M hops, pure version. Ack, stack overflow!
  Same with Cnc.hs

Well, well, with a simple tail-call optimization, Cnc.hs can do 5M in
two seconds.  WHOA Ghc can now work wonders on this code.  It's
beating the pants of the native haskell (language shootout)
implementation!  0.732 seconds for 50M!!!  Compared to 21.7 seconds
for the native haskell one on honor.  500M in 7.316 seconds!  Flat
memory usage (1.8mb), linear speed.  Spending 0.4% time in GC.  Ideal!

This counts as an "alternate" implementation... but that's the best
alternate implementation on the language shootout!

With the forkIO-based "call", 50M slows down by almost a factor of 10:
to 6.0s.  But that's fine, that's still a great time, and this is our parallel version.

The BEST part for this benchmark, is that there's no real reason not
to enable the tail-call optimization for the forkIO version as well!
Why fork another thread when your own thread is finished??  The result
is a parallel scheduler that also DOMINATES this benchmark.

 --> Returning to CncPure.hs
   It's hard to understand why this implementation has a stack leak.
   The scheduler loop is tail recursive.  It has the same problem both
   with simpleScheduler and with betterScheduler.  Laziness must be at fault.

  OK, I spammed the bang-pattern (!) and got it to work!  I banged
  basically everything in callSteps, mergeUpdates and simpleScheduler.
  Now the pure version can do 50M in 9.1 seconds with a constant 2mb
  memory.  (It's an order of magnitude worse even for 5M with
  memoization enabled.  Bounded memoization could elegantly combine
  the benefits... some kind of cache replacement policy.  What's the
  right data structure for it?)

  Narrowing it down...  Unstricting mergeUpdates breaks it
  again... Making it strict ONLY in the items component of the world
  argument passed to mergeUpdates does the trick.  Interesting, why is
  it needed on items but not tags?  Ah, because this program doesn't
  really use the item collections!  All the traffic was on the tag
  collections, but for each time round the scheduler, the world was
  accumulating layer after layer of meaningless item updates in the
  form of thunks!  Well the scheduler has to use tags, right?  So this
  can only happen with items...

  An alternative would be to make the MT/MI data constructors strict.
  There's really no point at all in postponing the computation of the
  outer IntMaps!!!  That's what I did.



-}
