{-# LANGUAGE RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}

-- 
-- , ScopedTypeVariables
--  FlexibleContexts
{- # OPTIONS glasgow-exts #-}


-- Haskell needs :t in the code!!!


import Data.Set as Set
import Data.Map as Map
import Data.IORef
import Unsafe.Coerce
import Control.Monad

--import Data.IntMap as Map
--type Foo = IntMap Float

------------------------------------------------------------
-- Type definitions:

-- The central abstraction is a (heterogeneous) group of item and tag collections.
type Collections = (Int, MatchedTagMap, MatchedItemMap)

-- I think this is WRONG.  Quantification should have a narrower scope.
data MatchedItemMap = forall a b. MI (Map (ItemColID a b) (ItemCol a b))
data MatchedTagMap  = forall a.   MT (Map (TagColID  a)   (TagCol a))

type TagColID  a   = Int
type ItemColID a b = Int

type TagCol a    = Set a
type ItemCol a b = Map a b

-- A STEP:
-- Produces a batch of new data to write, or blocks
type Step a = Collections -> a -> StepResult
data StepResult = Done ([NewTag], [NewItem])
                | forall a. Block (TagColID a) a
		  -- type-wise Block is the same as NewTag
data NewTag  = forall a.   Ord a => NT (TagColID  a)   a
data NewItem = forall a b. Ord a => NI (ItemColID a b) a b


-- A GRAPH:
-- Separate from the collections themselves, the graph keeps track of
-- the prescribed steps.  Needs ImpredicativeTypes:
--data Graph = G [forall a. (TagColID a, Step a)]
--data Graph = forall a. G [(TagColID a, Step a)]
--data Graph a = G [(TagColID a, Step a)]

--data MatchedStep = forall a. MS (TagColID a, Step a)
--type Graph = [MatchedStep]

-- Need it to be a map... but this type is wrong, as above:
data Graph = forall a. G (Map (TagColID a) (Step a))

------------------------------------------------------------
-- (Optional) type signatures for operations:

newCollections :: IO (IORef Collections)
newTagCol      :: IORef Collections -> IO (TagColID a)
newItemCol     :: IORef Collections -> IO (ItemColID a b)


get :: Ord a => Collections -> ItemColID a b -> a -> Maybe b

-- These are called by the step code and produce outbound tags and items:
put  :: Ord a => ItemColID a b -> a -> b -> NewItem
call :: Ord a => TagColID  a   -> a      -> NewTag


--------------------------------------------------------------------------------
-- Implementation:

newCollections = newIORef (0, MT Map.empty, MI Map.empty)
newTagCol ref = do (cnt, MT tags, items) <- readIORef ref		   
		   let newtags = Map.insert cnt Set.empty tags
		   writeIORef ref (cnt+1, MT newtags, items)
		   return cnt
newItemCol ref = do (cnt, tags, MI items) <- readIORef ref 	   
		    let newitems = Map.insert cnt Map.empty items
		    writeIORef ref (cnt+1, tags, MI newitems)
		    return cnt

--get (MT tmap, MI imap) (id :: ItemColID a b) tag = 
get (_, MT tmap, MI imap) id tag = 
  Map.lookup tag (unsafeCoerce (imap!id))

 -- Should we be strict in this outer lookup??
--   case imap ! id of 
--     items -> Map.lookup tag (unsafeCoerce items)


--  case Map.lookup id imap of 
--    Nothing -> Nothing
--    Just items -> Map.lookup tag (unsafeCoerce items)

-- Having problems with scopedtypevariables
--    Just items -> Map.lookup tag (items :: ItemCol a b)

-- Just accumulate puts:
put id tag item = NI id tag item
call id tag     = NT id tag 


-- SHOULD WE USE foldl' ???

-- This inserts new items and tags into a Collections object.

-- This is inefficient in that it looks up the tagCol/itemCol ID for
-- each update.  Ideally, steps would produce a more organized
-- "chunked" structure so that we could

-- Also, we could optimize this here by optimistically assuming that 

mergeUpdates cref newtags newitems =
    do (n, MT tags, MI items) <- readIORef cref
       let items' = foldl (\ acc (NI id k x) -> 
 			  -- DOESNT SEEM LIKE WE SHOULD NEED TWO unsafeCoerces!!
  			  let _acc = unsafeCoerce acc 
 			      col = Map.insert k x (_acc!id) in
  			  unsafeCoerce $ Map.insert id col _acc)
 	             items newitems
       let tags' = foldl (\ acc (NT id k) -> 
 			  let _acc = unsafeCoerce acc 
 			      col = Set.insert k (_acc!id) in
 			  unsafeCoerce $ Map.insert id col _acc)
 	             tags newtags
       writeIORef cref (n, MT tags', MI items')
       return ()

--        let itemloop acc [] = items
-- 	   itemloop acc ((NI id k x):tl) = 
-- 	       let _acc = unsafeCoerce acc 
-- 		   col = Map.insert k x (_acc!id)
-- 		   acc' = unsafeCoerce $ Map.insert id col _acc 
-- 	       in  itemloop acc' tl

--        let items'' = itemloop items newitems 
--        let tags' = tags 




--prescribe :: Graph a -> TagColID a -> Step a -> Graph a
--prescribe :: Graph -> TagColID a -> Step a -> Graph
--prescribe (G graph) tagsid step = G ((tagsid,step) : graph )

--prescribe :: Graph -> (TagColID a, Step a) -> Graph
--prescribe (G graph) pair = G (pair : graph )

--prescribe :: Graph -> MatchedStep -> Graph
--prescribe graph pair = (pair : graph )



--emptyGraph = []
--prescribe :: Graph -> TagColID a -> Step a -> Graph
--prescribe graph id step = MS (id,step) : graph 

emptyGraph = G Map.empty
prescribe :: TagColID a -> Step a -> Graph -> Graph
prescribe id step (G gmap) = G (Map.insert id (unsafeCoerce step) gmap)

-- Retrieve the steps from a graph:
--getSteps :: Graph -> TagColID a -> [Step a]

-- Serially run actions and make updates
serialScheduler cref (G graph) inittags = 
    do c <- readIORef cref
       -- Initially we should take ALL resident tags to be un-processed:
       -- Nevermind, we are lazy and require the user tell us:
       let schedloop [] = return ()
	   schedloop (NT id tag : tl) = 
--	     let 
	     case undefined of --step tag of          
	     Block id tag -> return ()
       return ()

--------------------------------------------------------------------------------
-- Test program:

type TI = TagColID Int
type II = ItemColID Int Int
incrStep :: TI -> (TI, II) -> Step Int
incrStep d1 (t2,d2) c tag = 
    case get c d1 tag of 
      Nothing -> Block d1 tag
      Just n ->  Done ([call t2 tag],
		       [put  d2 tag (n+1)])

test = -- Allocate collections:
    do cref <- newCollections 
       t1 <- newTagCol cref
       t2 <- newTagCol cref
       t3 <- newTagCol cref
       d1 <- newItemCol cref
       d2 <- newItemCol cref
       d3 <- newItemCol cref
       c  <- readIORef cref

       -- Initialize:
       mergeUpdates cref
		    []
		    [put d1 'a' 33,
		     put d1 'b' 100]

       let graph = 
	    prescribe t1 (incrStep d1 (t2,d2)) $
	    prescribe t2 (incrStep d2 (t3,d3)) $
	    emptyGraph

       let inittags = [call t1 'a',  call t1 'b']
--       serialScheduler cref graph inittags

       c  <- readIORef cref
       return (get c d3 'a', get c d3 'b') 
      


{-


-}

main = putStrLn "hello"



{-

poly_col.o(.text+0x54f): In function `sQa_info':
: undefined reference to `__stginit_containerszm0zi2zi0zi0_DataziMap_'
poly_col.o(.text+0x55b): In function `sQa_info':
: undefined reference to `__stginit_containerszm0zi2zi0zi0_DataziSet_'
collect2: ld returned 1 exit status



Prelude> :r
[1 of 1] Compiling Main             ( poly_col.hs, interpreted )
ghc: panic! (the 'impossible' happened)
  (GHC version 6.10.1 for x86_64-unknown-linux):
        readFilledBox a{tv a1rY} [box]

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

-}
