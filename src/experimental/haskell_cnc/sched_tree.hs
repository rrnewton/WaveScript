{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures #-}

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
	    (\_n -> do n <- get items _n
	               --cncPutStr$ "Running "++ show n ++"\n"
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
	    []  -> run 200000
	    [s] -> run (read s)

{-

NOTES:

[2009.08.12] 
Hmm, this doesn't seem to work with my global-work queue Cnc.hs version.



 -}