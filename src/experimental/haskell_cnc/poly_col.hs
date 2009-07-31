{-# LANGUAGE ExistentialQuantification #-}

-- 
-- RankNTypes

-- , ScopedTypeVariables, PatternSignatures
-- Also had that same 'impossible' crash on mac with 6.8.3

--
-- , ImpredicativeTypes
-- 
--  FlexibleContexts
{- # OPTIONS glasgow-exts #-}


-- Haskell needs :t in the code!!!


import Data.Set as Set
import Data.Map as Map
import Data.IORef
import Unsafe.Coerce
import Control.Monad
import qualified Data.IntMap as IntMap

import System.IO.Unsafe
import Debug.Trace
import Data.Maybe

--import Data.IntMap as Map
--type Foo = IntMap Float

------------------------------------------------------------
-- Type definitions:

-- The central abstraction is a (heterogeneous) group of item and tag collections.
type Collections = (Int, MatchedTagMap, MatchedItemMap)


showcol (n, MT tmap, MI imap) =
  show (n, Map.size tmap, IntMap.keys imap, Map.keys foo, Map.elems foo)
 where 
    foo = unsafeCoerce $ (IntMap.!) imap 0 :: ItemCol Char Int

-- I think this is WRONG.  Quantification should have a narrower scope.
data MatchedItemMap = forall a b. Ord a => MI (IntMap.IntMap (ItemCol a b))
data MatchedTagMap  = forall a.   MT (Map (TagColID  a)   (TagCol a))

type TagColID  a   = Int
data ItemColID a b = ICID Int
type TagCol    a   = Set a
type ItemCol   a b = Map a b

-- Produces a batch of new data to write, or blocks
type Step a = Collections -> a -> StepResult
data StepResult = Done ([NewTag], [NewItem])
                | forall a. Ord a => Block (TagColID a) a
		  -- type-wise Block is the same as NewTag
data NewTag  = forall a.   Ord a => NT (TagColID  a)   a
data NewItem = forall a b. Ord a => NI (ItemColID a b) a b

-- Need it to be a map... but this type is wrong, as above:
data Graph = forall a. G (Map (TagColID a) [Step a])

------------------------------------------------------------
-- (Optional) type signatures for operations:

newCollections :: IO (IORef Collections)
newTagCol      :: IORef Collections -> IO (TagColID a)
newItemCol     :: IORef Collections -> IO (ItemColID a b)


-- These are called by the step code and produce outbound tags and items:
put  :: Ord a => ItemColID a b -> a -> b -> NewItem
call :: Ord a => TagColID  a   -> a      -> NewTag


--------------------------------------------------------------------------------
-- Implementation:

--dummy = Map.empty :: Map (ItemColID Int Int) (ItemCol Int Int)
dummy = IntMap.empty :: IntMap.IntMap (ItemCol Int Int)

newCollections = newIORef (0, MT Map.empty, MI dummy)
newTagCol ref = do (cnt, MT tags, items) <- readIORef ref		   
		   let newtags = Map.insert cnt Set.empty tags
		   writeIORef ref (cnt+1, MT newtags, items)
		   return cnt
newItemCol ref = do (cnt, tags, MI items) <- readIORef ref 	   
		    let newitems = IntMap.insert cnt Map.empty items
		    writeIORef ref (cnt+1, tags, MI newitems)
		    return (ICID cnt)

magic :: ItemColID a b -> ItemCol c d -> (a->c, d->b)
magic id col = (unsafeCoerce, unsafeCoerce)


{-# NOINLINE get #-}
get :: Ord a => Collections -> ItemColID a b -> a -> Maybe b
get (_, MT tmap, MI imap) id tag = 
  unsafePerformIO (putStrLn $ "  GETTING "++ show (unsafeCoerce tag::Char)) `seq`
  let ICID n = id 
      itemcol = (IntMap.!) imap n
      (castkey,castback) = magic id itemcol in
  case
      --trace ("LOOKUP " ++ (show $ (unsafeCoerce (Map.lookup (castkey tag) itemcol :: Maybe b) :: Maybe Int))) $ 
      --unsafePerformIO (putStrLn $ "  tag again "++ show (castkey tag)) `seq`
      Map.lookup (castkey tag) itemcol of
    Nothing -> trace " DAMN " $ Nothing
    Just d  -> trace (" YAY************ " ++ show (unsafeCoerce d :: Int)) $ Just (castback d)


put id tag item = NI id tag item -- Just accumulate puts as data
call id tag     = NT id tag 

bla = 
    do cref <- newCollections 
       d1 <- newItemCol cref :: IO (ItemColID Char Int)
       modifyIORef cref $ 
	 mergeUpdates [] [put d1 'z' 33, put d1 'b' 100]
       c  <- readIORef cref
       putStrLn "WTF is happening"
       putStrLn $ showcol c
       let !foo = trace "GET starting" $ get c d1 'b'
       putStrLn "almost done"
       --return $ trace ("foo " ++ show (foo::Maybe Int)) $ foo
--       return $ (fromJust foo) + 0
       return $ foo


-- This inserts new items and tags into a Collections object.
--
-- This is inefficient in that it looks up the tagCol/itemCol ID for
-- each update.  Ideally, steps would produce a more organized
-- "chunked" structure so that we could
--
-- Also, we could optimize this here by optimistically assuming that a
-- batch of updates are likely to the same collection.

magic2 :: ItemColID a b -> ItemCol c d ->  (a -> c, b -> d)
--	  (a -> c, b -> d, ItemColID a b -> ItemColID c d)
magic2 id col = (unsafeCoerce, unsafeCoerce)

--mergeUpdates :: IORef Collections -> [NewTag] -> [NewItem] -> IO ()
mergeUpdates :: [NewTag] -> [NewItem] -> Collections -> Collections
--mergeUpdates cref newtags newitems =
mergeUpdates newtags newitems (n, MT tags, MI items) =
       -- SHOULD WE USE foldl' ???
       let items' = foldl (\ acc (NI id k x) -> 
  			    let ICID n = id 
			        (castkey, castval) = magic2 id itemcol 
			        itemcol = (IntMap.!) acc n
 			        col = Map.insert (castkey k) (castval x) itemcol 
			    in
  			    IntMap.insert n col acc 
			  )
 	             items newitems in

-- Even THIS doesn't work:
--       let items' = foldl (\ acc newitem -> undefined) items [] in
--       let loop newitems = undefined
--       in
--       let items' = loop newitems inC
--        let tags' = foldl (\ acc (NT id k) -> 
--  			  let _acc = unsafeCoerce acc 
--  			      col = Set.insert k (_acc!id) in
--  			  unsafeCoerce $ Map.insert id col _acc)
--  	             tags newtags in
       (n, MT tags, MI items')
--       writeIORef cref 
--       return ()

{-
emptyGraph = G Map.empty
prescribe :: TagColID a -> Step a -> Graph -> Graph
prescribe id step (G gmap) = G (Map.insertWith (++) id [unsafeCoerce step] gmap)

-- Retrieve the steps from a graph:
getSteps :: Graph -> TagColID a -> [Step a]
getSteps (G graph) id = Map.findWithDefault [] id (unsafeCoerce graph)

-- Serially run actions and make updates
serialScheduler graph inittags cols = schedloop cols [] inittags
--    do c <- readIORef cref       
--	    schedloop c [] inittags
       -- Initially we should take ALL resident tags to be un-processed:
       -- Nevermind, we are lazy and require the user tell us:
 where schedloop c [] [] = c
       -- FIXME: TEMP HACK -- just try all the blocked ones when we run out of other stuff:
       -- TODO: wake up blocked steps intelligently when the output is produced.
       schedloop c blocked [] = schedloop c [] blocked
       schedloop c blocked (NT id tag : tl) = 
	     foldl (\ acc step -> 
		    case step acc tag of
		      Block id tag -> schedloop acc ((NT id tag):blocked) tl
		      Done (newtags, newitems) -> 
		        schedloop (mergeUpdates newtags newitems acc)
		                  blocked (newtags++tl) -- FIXME: SUPPRESS pre-existing tags. Currently the tag collections do NOTHING
		   )
	           c (getSteps graph id)


--------------------------------------------------------------------------------
-- Test program:

type TI = TagColID  Char
type II = ItemColID Char Int
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
       d1 <- newItemCol cref :: IO (ItemColID Char Int)
       d2 <- newItemCol cref
       d3 <- newItemCol cref

       -- Initialize:
       modifyIORef cref $ 
	 mergeUpdates [] [put d1 'a' 33, 
			  put d1 'b' 100]
{-
       let graph = 
	    prescribe t1 (incrStep d1 (t2,d2)) $
	    prescribe t2 (incrStep d2 (t3,d3)) $
	    emptyGraph

       let inittags = [call t1 'a',  call t1 'b']

       modifyIORef cref $ serialScheduler graph inittags

-}
       c  <- readIORef cref
       let foo = get c d1 'a' :: Maybe Int
       return $ (d1, get c d1 'a', get c d1)
--       return (get c d1 'a', get c d1 'b') 
--       return (get c d3 'a', get c d3 'b') 
      
-}

--main = putStrLn "hello"


