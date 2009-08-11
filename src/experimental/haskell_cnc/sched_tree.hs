{-# LANGUAGE ExistentialQuantification, FlexibleInstances, BangPatterns, MagicHash, ScopedTypeVariables, PatternSignatures #-}

{-
 #include "Cnc2_wmagic.hs"
 #include "Cnc3.hs"
-}

import CncPure
--import Cnc3

-- A simple scheduler test that creates a tree of exponentially
-- expanding numbers of step executions (as though it were a binary
-- tree).

-- The item collection mirrors the tag collection and is pretty much pointless.

main = 
  do v <- runGraph $  
       do tags  :: TagCol Int      <- newTagCol()
	  items :: ItemCol Int Int <- newItemCol() 

	  prescribe tags 
	    (\_n -> do n <- get items _n
	               put items (n*2) (n*2)
	               put items (n+1) (n+1)
	               call tags (n*2)
	               call tags (n+1))

	  initialize $ call tags 1
	  finalize $ get items 399
	  --finalize $ return () 
	  
     putStrLn (show v)
