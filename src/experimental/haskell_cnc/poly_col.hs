{-# LANGUAGE ExistentialQuantification
  #-}
{-
   , FlexibleContexts
   , RankNTypes
   , ScopedTypeVariables, PatternSignatures
   , ImpredicativeTypes
   , FlexibleContexts
  OPTIONS glasgow-exts
-}

-- Haskell needs :t in the code!!!

import Data.Set as Set
import Data.Map as Map
import Data.IORef
import Unsafe.Coerce
import Control.Monad
import qualified Data.IntMap as IntMap
import Debug.Trace

------------------------------------------------------------
-- Type definitions:

-- The central abstraction is a (heterogeneous) group of item and tag collections.
type Collections = (Int, MatchedTagMap, MatchedItemMap)

data MatchedItemMap = forall a b. Ord a => MI (IntMap.IntMap (ItemCol a b))
data MatchedTagMap  = forall a.   Ord a => MT (IntMap.IntMap  (TagCol a))

data TagColID  a   = TCID Int deriving (Ord, Eq)
data ItemColID a b = ICID Int
type TagCol    a   = Set a
type ItemCol   a b = Map a b

-- Produces a batch of new data to write, or blocks
type Step a = Collections -> a -> StepResult
data StepResult = Done ([NewTag], [NewItem])
                | forall a b. Ord a => Block (ItemColID a b) a
		  -- type-wise Block is the same as NewTag
data NewTag  = forall a.   Ord a => NT (TagColID  a)   a
data NewItem = forall a b. Ord a => NI (ItemColID a b) a b

-- Need it to be a map... but this type is wrong, as above:
data Graph = forall a. G (IntMap.IntMap [Step a])

------------------------------------------------------------
-- (Optional) type signatures for operations:

newCollections :: IO (IORef Collections)
newTagCol      :: IORef Collections -> IO (TagColID a)
newItemCol     :: IORef Collections -> IO (ItemColID a b)


-- These are called by the step code and produce outbound tags and items:
put  :: Ord a => ItemColID a b -> a -> b -> NewItem
call :: Ord a => TagColID  a   -> a      -> NewTag

get  :: Ord a => Collections -> ItemColID a b -> a -> Maybe b


--------------------------------------------------------------------------------
-- Implementation:

--dummy = Map.empty :: Map (ItemColID Int Int) (ItemCol Int Int)
dummy  = IntMap.empty :: IntMap.IntMap (ItemCol () ())
dummy2 = IntMap.empty :: IntMap.IntMap (TagCol ())

newCollections = newIORef (0, MT dummy2, MI dummy)
newTagCol ref = do (cnt, MT tags, items) <- readIORef ref		   
		   let newtags = IntMap.insert cnt Set.empty tags
		   writeIORef ref (cnt+1, MT newtags, items)
		   return (TCID cnt)
newItemCol ref = do (cnt, tags, MI items) <- readIORef ref 	   
		    let newitems = IntMap.insert cnt Map.empty items
		    writeIORef ref (cnt+1, tags, MI newitems)
		    return (ICID cnt)

magic :: ItemColID a b -> ItemCol c d -> (a->c, d->b)
magic id col = (unsafeCoerce, unsafeCoerce)


{-# NOINLINE get #-}
get (_, MT tmap, MI imap) id tag = 
  let ICID n = id 
      itemcol = (IntMap.!) imap n
      (castkey,castback) = magic id itemcol in
   case Map.lookup (castkey tag) itemcol of
    Nothing -> Nothing
    Just d  -> Just (castback d)

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
       return $ foo

magic2 :: ItemColID a b -> ItemCol c d ->  (a -> c, b -> d)
magic2 id col = (unsafeCoerce, unsafeCoerce)

magic3 :: TagColID a -> TagCol b ->  (a -> b)
magic3 id col = (unsafeCoerce)

-- This inserts new items and tags into a Collections object.
--
-- This is inefficient in that it looks up the tagCol/itemCol ID for
-- each update.  Ideally, steps would produce a more organized
-- "chunked" structure so that we could
--
-- Also, we could optimize this here by optimistically assuming that a
-- batch of updates are likely to the same collection.
--mergeUpdates :: IORef Collections -> [NewTag] -> [NewItem] -> IO ()
mergeUpdates :: [NewTag] -> [NewItem] -> Collections -> Collections
mergeUpdates newtags newitems (n, MT tags, MI items) =
       -- SHOULD WE USE a strcict foldl' ???
       trace (" MERGING " ++ show (length newitems))$
       let items' = foldl (\ acc (NI id k x) -> 
			    trace (" New item "++ show (unsafeCoerce k::Char)) $
  			    let ICID n = id 
			        (castkey, castval) = magic2 id itemcol 
			        itemcol = (IntMap.!) acc n
			        key = (castkey k)
 			        --newcol = Map.insert key (castval x) itemcol 
			        --- This works!!!! (if we force the key to be a char) 
			        -- Somehow the key is getting messed up, and probably turned to unit:
			        newcol = unsafeCoerce $ Map.insert (unsafeCoerce key::Char) (castval x) (unsafeCoerce itemcol) 
			    in
			    trace ("  >> the one we retrieved, had #items= " ++ (show (Map.size itemcol))) $
			    trace ("  >> and the new one.....  had #items= " ++ (show (Map.size newcol))) $
			    trace ("  >> Ok, the key was: " ++ (show (unsafeCoerce key::Char))) $
			    trace (" inserted, now collection " ++ (show (Map.keys ((unsafeCoerce newcol) :: Map Char Int)))) $
  			    IntMap.insert n newcol acc)
 	             items newitems in

       let tags' = foldl (\ acc (NT id k) -> 
  			    let TCID n = id 
			        (castkey) = magic3 id tagcol 
			        tagcol = (IntMap.!) acc n
 			        col = Set.insert (castkey k) tagcol 
			    in
  			    IntMap.insert n col acc)
 	             tags newtags in
       (n, MT tags', MI items')

magic4 :: TagColID a -> IntMap.IntMap [Step b] -> IntMap.IntMap [Step a]
magic4 id col = (unsafeCoerce col)

--prescribe :: Ord (TagColID a) => TagColID a -> Step a -> Graph -> Graph

emptyGraph = G IntMap.empty
--prescribe id step (G gmap) = G (Map.insertWith (++) id [unsafeCoerce step] gmap)

prescribe :: Ord a => TagColID a -> Step a -> Graph -> Graph
prescribe id step (G gmap) = 
    case id of 
     TCID n ->
       G (IntMap.insertWith (++) n [step] $ magic4 id gmap)

-- Retrieve the steps from a graph:
getSteps  :: Graph -> TagColID a -> [Step a]
--getSteps (G graph) id = Map.findWithDefault [] id (unsafeCoerce graph)

getSteps (G gmap) id = 
    case id of 
     TCID n -> IntMap.findWithDefault [] n (magic4 id gmap)

-- Serially run actions and make updates
serialScheduler graph inittags cols = schedloop cols [] inittags
--    do c <- readIORef cref       
--	    schedloop c [] inittags
       -- Initially we should take ALL resident tags to be un-processed:
       -- Nevermind, we are lazy and require the user tell us:
 where schedloop c [] [] = c
       -- FIXME: TEMP HACK -- just try all the blocked ones when we run out of other stuff:
       -- TODO: wake up blocked steps intelligently when the output is produced.
       --schedloop c blocked [] = schedloop c [] blocked
       schedloop c blocked [] = error "err ran out of tags but have blocked..."
       schedloop c blocked (hd : tl) = 
	   case trace ("  Looping... " ++ show (length tl)) $ hd of 
	    NT id tag ->
	     foldl (\ acc step -> 
		    case step acc tag of
		      -- FIXME: We don't YET track what item collection we blocked on.
		      Block d_id tag -> schedloop acc (hd:blocked) tl
		      Done (newtags, newitems) -> 
		        schedloop (mergeUpdates newtags newitems acc)
		                  blocked (newtags++tl) -- FIXME: SUPPRESS pre-existing tags. Currently the tag collections do NOTHING
		   )
	           c (getSteps graph id)


--------------------------------------------------------------------------------
-- Test program:

type TI = TagColID  Char
type II = ItemColID Char Int
incrStep :: II -> (TI, II) -> Step Char
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

       -- Initialize:
       modifyIORef cref $ 
	 mergeUpdates [] [put d1 'a' 33, 
			  put d1 'b' 100]

       let graph = 
	    prescribe t1 (incrStep d1 (t2,d2)) $
	    prescribe t2 (incrStep d2 (t3,d3)) $
	    emptyGraph

       let inittags = [call t1 'a',  call t1 'b']

       putStrLn "About to start scheduler...\n"

       --modifyIORef cref $ serialScheduler graph inittags

       putStrLn " -- FINISHED SCHEDULER\n"

       c  <- readIORef cref
       let foo = get c d1 'a' :: Maybe Int
--       return $ (d1, get c d1 'a', get c d1)
--       return "BLAH"

--       return (get c d3 'a', get c d3 'b') 

--       return (get c d1 'a', get c d1 'b') 
       putStrLn $ showcol c
       return (get c d1 'a') 
      
--main = putStrLn "hello"


showcol (n, MT tmap, MI imap) =
  show (n, IntMap.size tmap, IntMap.keys imap, 
	Map.keys foo, 
	Map.elems foo)
 where 
    -- Hack -- pull out the first item collection:
    foo = (unsafeCoerce $ (IntMap.!) imap 3) :: ItemCol Char Int


