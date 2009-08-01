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

data MatchedItemMap = forall a b. MI (IntMap.IntMap (ItemCol a b))
data MatchedTagMap  = forall a.   MT (IntMap.IntMap  (TagCol a))

data TagColID  a   = TCID Int deriving (Ord, Eq)
data ItemColID a b = ICID Int
type TagCol    a   = Set a
type ItemCol   a b = Map a b

-- Produces a batch of new data to write, or blocks
type Step a = a -> Collections -> StepResult
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

newCollections = newIORef (0, MT IntMap.empty, MI IntMap.empty)
newTagCol ref = do (cnt, MT tags, items) <- readIORef ref		   
		   let newtags = IntMap.insert cnt Set.empty tags
		   writeIORef ref (cnt+1, MT newtags, items)
		   return (TCID cnt)
newItemCol ref = do (cnt, tags, MI items) <- readIORef ref 	   
		    let newitems = IntMap.insert cnt Map.empty items
		    writeIORef ref (cnt+1, tags, MI newitems)
		    return (ICID cnt)


magic :: ItemColID a b -> ItemCol c d -> ItemCol a b
magic id = (unsafeCoerce)

get (_, _, MI imap) id tag = 
  let ICID n = id 
      badcol  = (IntMap.!) imap n
      goodcol = magic id badcol
   in
   case Map.lookup tag goodcol of
    Nothing -> Nothing
    Just d  -> Just d

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

moremagic :: IntMap.IntMap (ItemCol a b) -> ItemCol c d -> ItemCol a b
moremagic id = (unsafeCoerce)

tmagic :: TagColID a -> TagCol c -> TagCol a
tmagic id = (unsafeCoerce)

mostmagic :: IntMap.IntMap (TagCol a) -> TagCol c -> TagCol a
mostmagic id = (unsafeCoerce)


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
       trace ("   MERGING "++ show (length newtags)++ " tags " ++ show (length newitems) ++ " items" )$
       let items' = foldl (\ acc (NI id k x) -> 
			    trace ("    New item "++ show (unsafeCoerce k::Char)) $
  			    let ICID n = id 
			        badcol = (IntMap.!) acc n
			        goodcol = magic id badcol
 			        newcol = moremagic acc $ Map.insert k x goodcol
			    in
			    trace ("  >> the one we retrieved, had #items= " ++ (show (Map.size goodcol))) $
			    trace ("  >> and the new one.....  had #items= " ++ (show (Map.size newcol))) $
			    trace (" inserted, now collection " ++ (show (Map.keys ((unsafeCoerce newcol) :: Map Char Int)))) $
  			    IntMap.insert n newcol acc)
 	             items newitems in

       let tags' = foldl (\ acc (NT id k) -> 
  			    let TCID n = id 
			        badcol = (IntMap.!) acc n
			        goodcol = tmagic id badcol
 			        newcol = mostmagic acc $ Set.insert k goodcol
			    in
  			    IntMap.insert n newcol acc)
 	             tags newtags in
       (n, MT tags', MI items')

megamagic :: TagColID a -> IntMap.IntMap [Step b] -> IntMap.IntMap [Step a]
megamagic id col = (unsafeCoerce col)

--prescribe :: Ord (TagColID a) => TagColID a -> Step a -> Graph -> Graph

emptyGraph = G IntMap.empty
--prescribe id step (G gmap) = G (Map.insertWith (++) id [unsafeCoerce step] gmap)

prescribe :: Ord a => TagColID a -> Step a -> Graph -> Graph
prescribe id step (G gmap) = 
    case id of 
     TCID n ->
       G (IntMap.insertWith (++) n [step] $ megamagic id gmap)

-- Retrieve the steps from a graph:
getSteps  :: Graph -> TagColID a -> [Step a]
getSteps (G gmap) id = 
    case id of 
     TCID n -> IntMap.findWithDefault [] n (megamagic id gmap)


-- Returns thunks representing the result of the steps:
callSteps  :: Graph -> TagColID a -> a -> [Collections -> StepResult]
callSteps (G gmap) id tag = 
    case id of 
     TCID n -> Prelude.map (\fn -> fn tag) $ 
	       IntMap.findWithDefault [] n (megamagic id gmap)

char x = unsafeCoerce x :: Char

-- Serially run actions and make updates
-- serialScheduler graph inittags cols = schedloop cols [] inittags
-- --    do c <- readIORef cref       
-- --	    schedloop c [] inittags
--        -- Initially we should take ALL resident tags to be un-processed:
--        -- Nevermind, we are lazy and require the user tell us:
--  where schedloop c [] [] = c
--        -- FIXME: TEMP HACK -- just try all the blocked ones when we run out of other stuff:
--        -- TODO: wake up blocked steps intelligently when the output is produced.
--        --schedloop c blocked [] = schedloop c [] blocked
--        schedloop c blocked [] = error "err ran out of tags but have blocked..."
--        schedloop c blocked (hd : tl) = 
-- 	   case trace ("\n  Looping... " ++ show (1 + length tl)) $ hd of 
-- 	    NT id tag ->
-- 	     trace (case id of TCID n -> "      *** Executing tagcol "++ show n ++" tag: "++ show (char tag)) $ 

-- 	     -- For each step triggered by this tag, we do depth-first traversals:
-- 	     foldl (\ acc step -> 
-- 		    case step acc tag of
-- 		      -- FIXME: We don't YET track what item collection we blocked on.
-- 		      Block d_id tag -> trace (" ... Blocked ... ") $
-- 		                        schedloop acc (hd:blocked) tl
-- 		      Done (newtags, newitems) -> 
-- 		        schedloop (mergeUpdates newtags newitems acc)
-- 		                  blocked (newtags++tl) -- FIXME: SUPPRESS pre-existing tags. Currently the tag collections do NOTHING
-- 		   )
-- 	           c (getSteps graph id)



serialScheduler graph inittags cols = schedloop cols [] inittags []
 where schedloop c [] [] []  = c
       -- FIXME: TEMP HACK -- just try all the blocked ones when we run out of other stuff:
       -- TODO: wake up blocked steps intelligently when the output is produced.
       --schedloop c blocked [] = schedloop c [] blocked
       schedloop c blocked [] [] = error "err ran out of tags but have blocked..."

       schedloop c blocked (hd : tl) [] = 
	   case trace ("\n  Looping (popping a new tag)... " ++ show (1 + length tl)) $ hd of 
	    NT id tag ->
	     -- For each step triggered by this tag, we do depth-first traversals:	           	   
	     --let (c,blocked') = steploop c (getSteps graph id) in
	     --schedloop c tag (blocked' ++ blocked) tl
	     schedloop c blocked tl (callSteps graph id tag)

       schedloop c blocked tags (step:tl) = 
	   --trace (case id of TCID n -> "      *** Executing tagcol "++ show n ++" tag: "++ show (char tag)) $ 
	   case step c of
	     -- FIXME: We don't YET track what item collection we blocked on.
	     Block d_id tag -> trace (" ... Blocked ... ") $
			       schedloop c (step:blocked) tags tl
	     Done (newtags, newitems) -> 
		 schedloop (mergeUpdates newtags newitems c)
		           blocked (newtags++tags) tl
	 -- FIXME: SUPPRESS pre-existing tags. Currently the tag collections do NOTHING



--------------------------------------------------------------------------------
-- Test program:

type TI = TagColID  Char
type II = ItemColID Char Int
incrStep :: II -> (TI, II) -> Step Char
incrStep d1 (t2,d2) tag c = 
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

       case graph of G g -> putStrLn $ "Graph Size... " ++ (show $ IntMap.size g)

       let inittags = [call t1 'a',  call t1 'b']
--       let inittags = [call t1 'b', call t1 'a']

       putStrLn "About to start scheduler...\n"
       modifyIORef cref $ serialScheduler graph inittags

       c  <- readIORef cref
       let foo = get c d1 'a' :: Maybe Int

       putStrLn $ showcol c
       putStrLn $ "  d1: " ++ show (get c d1 'a', get c d1 'b') 
       putStrLn $ "  d2: " ++ show (get c d2 'a', get c d2 'b') 
       putStrLn $ "  d3: " ++ show (get c d3 'a', get c d3 'b') 
       return ()


showcol (n, MT tmap, MI imap) =
  show (n, IntMap.size tmap, IntMap.keys imap, 
	Map.keys foo, 
	Map.elems foo)
 where 
    -- Hack -- pull out the first item collection:
    foo = (unsafeCoerce $ (IntMap.!) imap 3) :: ItemCol Char Int


