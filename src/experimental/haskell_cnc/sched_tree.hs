{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures, NamedFieldPuns, RecordWildCards #-}

#define MEMOIZE
#define REPEAT_PUT_ALLOWED
#include "stub.h"

-- A simple scheduler test that creates a tree of exponentially
-- expanding numbers of step executions (as though it were a binary
-- tree).

-- The item collection mirrors the tag collection and is pretty much pointless.

run limit = 
  do v <- runGraph $  
       do tags  :: TagCol Int      <- newTagCol()
	  items :: ItemCol Int Int <- newItemCol() 

	  prescribe tags 
	    (\_n -> do 
--	               cncPutStr$ "Running "++ show _n ++"\n"
	               n <- get items _n
--	               cncPutStr$ "    got "++ show n ++"\n"
	               if n >= limit
	                then return ()
	                else do put items (n+1) (n+1)
	                        call tags (n+1)
	                        put items (n*2) (n*2)
	                        call tags (n*2)
	    )
	  initialize $ 
	     -- TEST: should be fine to throw an extra tag out there which will block:
	     do call tags 5
		call tags 1
		put items 1 1

	  finalize $ get items 100
	  --finalize $ return () 
-- 	  finalize $ do x <- get items 9
-- 			l <- itemsToList items
-- 			--return (length l)
-- 			return (l)
	  
     putStrLn (show v)


main = do args <- System.getArgs 
	  case args of 
--	    []  -> run 200000
--	    []  -> run 10000
	    []  -> run 200 
	    [s] -> run (read s)

{-

NOTES:

[2009.08.12] 
   Hmm, this doesn't seem to work with my global-work queue Cnc.hs version.
   Ah, ok I added a REPEAT_PUT_ALLOWED flag.

 (Using io/Map, scheduler 5:)
   This runs ever so slightly faster (8s for 9M limit rather than 8.75)
   with 4 threads than with 1. (And it gets 8.29s compiled without -threaded).
 Also:
   Wish hashtables -threaded and 1 thread: 14.1
   With                         4 threads: 13.28

 Pure: (currently not parallel)
   nothreads: 6.65
   1 thread:  6.7
   4 threads: 6.48

[2009.08.25]

Note... for testing my new parallel CncPure.hs, this DOES involve
blocking/reactivation (under scheduler 3).

Currently this test fails due to finalization code blocking....

Yuck... it may be nondetermistic... 
  Well, it seems grain/threads = 2 is working....
  But 1/1 doesn't work!! That's odd.


 -}