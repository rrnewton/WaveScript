{-# LANGUAGE ExistentialQuantification  #-}

{-
  This first version is serial.
  It uses a single state thread to track all the tag/item collection state.

UNFINISHED -- this implementation doesn't yet do blocking GETS. [2009.08.12]

 -}

import Control.Monad.ST
import Data.STRef
import Data.Set as Set
import Data.Map as Map
import System.IO.Unsafe

------------------------------------------------------------
-- Type definitions:

-- A tag collection has a buffer of newly written tags.
-- It also has a unique identifier.
type TagColID = Int
type TagCol  s a   = STRef s (TagColID, Set a, Set a)

type ItemCol s a b = STRef s (Map a b)
type Step s a = a -> CncCode s ()

-- The monad stores all the information in the tag/item collections
-- It's a state thread.  There only needs to be ONE state thread currently.
type CncCode s a = ST s a

-- A graph data structure keeps track of the steps that are prescribed
-- everything that is needed by the scheduler.
-- Do we need a different monad for wiring together the graph??
-- Not in this version...
data Graph s = Graph (STRef s (Map TagColID [MatchedPair s]))

-- Tricky existential types:
-- Here's a tag collection and associated step that "match" tag-types:
data MatchedPair s = forall t. Ord t => MP (TagCol s t, Step s t)


------------------------------------------------------------
-- (Optional) type signatures for operations:

-- You can mostly ignore 's' type variables.
newTagCol  :: TagColID -> CncCode s (TagCol s a)
--newItemCol :: CncCode s (ItemCol s a b)
--newTagCol  :: () -> CncCode s (TagCol s a)
newItemCol :: () -> CncCode s (ItemCol s a b)
call :: Ord a           => TagCol  s a   -> a      -> CncCode s ()
put  :: Ord a           => ItemCol s a b -> a -> b -> CncCode s ()
get  :: (Ord a, Show a) => ItemCol s a b -> a      -> CncCode s b

newGraph  :: CncCode s (Graph s)
_prescribe :: Graph s -> MatchedPair s -> CncCode s ()

--------------------------------------------------------------------------------
-- Implementation:

-- counter = runST $ newSTRef 0
-- newTagCol () = do cnt <- counter; 
-- 		  writeSTRef counter (cnt+1)
-- 		  newSTRef (cnt+1, Set.empty, Set.empty)

newTagCol cnt = newSTRef (cnt+1, Set.empty, Set.empty)
newItemCol () = newSTRef Map.empty

call col tag = 
    do (id, frontset,backset) <- readSTRef col
       writeSTRef col (id, Set.insert tag frontset, backset)
put col tag item = 
    do map <- readSTRef col
       writeSTRef col (Map.insert tag item map)
get col tag = 
    do map <- readSTRef col
       return $ Map.findWithDefault 
		  (error ("get item: tag not found " ++ show tag)) tag map

newGraph = do ref <- newSTRef (Map.empty :: Map TagColID [MatchedPair s])
	      return (Graph ref)

_prescribe (Graph graph) (MP (tagc,step)) = 
    do stepmap <- readSTRef graph
       (id,frnt,bck) <- readSTRef tagc
       let ls = Map.findWithDefault ([]::[MatchedPair s]) id stepmap 
       let pr = MP (tagc,step) 
       writeSTRef graph (Map.insert id (pr:ls) stepmap)
       return ()

runSimpleScheduler (Graph graph) = 
    do    stepmap <- readSTRef graph
	  maybePrint ("Size of map " ++ show (Map.size stepmap) ++"\n")
	  allempty <-
	   Map.fold (\ ls acc ->   -- For each tag collection:
		    case head ls of
		     (MP (tags,_)) -> 
		        do (id,frnt,bck) <- readSTRef tags
		           bool <- acc
		           let newbool = bool && Set.null frnt
		           -- For each step prescribed by this tag collection:
		           foldr (\ (MP (tags,step)) (acc) ->
				  do (id,frnt,bck) <- readSTRef tags
				     -- Invoke the step:
				     Set.fold (\ tag acc -> do acc; step tag)
				              acc frnt)
		                 (return ()) ls
		           -- After we're done consuming the fresh tags, clear them:
		           writeSTRef tags (id, Set.empty, Set.union frnt bck)
		           maybePrint ("Finished tag collection " ++ show id ++"\n")
		           return newbool)
		   (return True)
         	   stepmap	  
	  if allempty
	   then maybePrint ("All tag collection inboxes empty.  Finished.\n")
	   else runSimpleScheduler (Graph graph)


maybePrint str = return ()
--maybePrint = unsafeIOToST . putStr

--------------------------------------------------------------------------------
-- Common interface for interoperating with my alternate implementations:

-- For this implementation we don't have separate monads for CncCode and GraphCode.
type GraphCode s a = CncCode s a

-- Embed CncCode in the graph construction program:
initialize :: CncCode s a -> GraphCode s a
initialize x = x

finalize :: CncCode s a -> GraphCode s a
finalize x = x 

prescribe :: Ord a => TagCol s a -> (a -> CncCode s ()) -> GraphCode s ()
prescribe tags step = 
  undefined


-- Bring us from the graph monad back to the IO monad:
runGraph = (return :: a -> IO a) . runST 

--itemsToList :: ItemCol a b -> CncCode [(a,b)]
itemsToList col = 
 do map <- readSTRef col
    return (Map.toList map)

--------------------------------------------------------------------------------
-- Test program:

incrStep :: (Ord a, Show a, Num b) =>
            ItemCol s a b -> (TagCol s a, ItemCol s a b) -> a -> CncCode s ()
incrStep d1 (t2,d2) tag = 
 do val <- get d1 tag 
    unsafeIOToST $ putStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
    put d2 tag (val + 1)
    call t2 tag

test = -- Allocate collections:
    do t1 <- newTagCol 0
       t2 <- newTagCol 1
       t3 <- newTagCol 2
       d1 <- newItemCol()
       d2 <- newItemCol()
       d3 <- newItemCol()
       -- Initialize:
       put d1 'a' 33
       put d1 'b' 100
       call t1 'a'
       call t1 'b'
       -- Build and execute the graph:
       graph <- newGraph
       _prescribe graph (MP (t1, incrStep d1 (t2,d2)))		 
       _prescribe graph (MP (t2, incrStep d2 (t3,d3)))
       runSimpleScheduler graph
       result1 <- get d3 'a'
       result2 <- get d3 'b'
       return (result1, result2) 

{-
test2 = 
 do v <- runGraph $ do
        t1 <- newTagCol 0
        t2 <- newTagCol 1
        t3 <- newTagCol 2
        d1 <- newItemCol()
        d2 <- newItemCol()
        d3 <- newItemCol()
		  
	initialize $ do put d1 'a' 33
 			put d1 'b' 100
			call t1 'b'
			call t1 'a'

        let incrStep d1 (t2,d2) tag = 
	     do n <- get d1 tag
	        put d2 tag (n+1)
	        call t2 tag

        prescribe t1 (incrStep d1 (t2,d2))
 	prescribe t2 (incrStep d2 (t3,d3))

        -- Get some of the results:
	finalize $ 
	  do a <- itemsToList d1
	     b <- itemsToList d2
	     c <- itemsToList d3
	     return (a,b,c)
		      
    putStrLn ("Final: "++ show v)
-}

go = runGraph test


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

