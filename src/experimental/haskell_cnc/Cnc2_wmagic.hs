{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , PatternSignatures
  #-}
{-
   , FlexibleContexts
   , RankNTypes
   , ImpredicativeTypes
   , FlexibleContexts

  OPTIONS glasgow-exts

   -funbox-strict-fields
-}

-- Haskell needs :t in the code!!!

import Data.Set as Set
import Data.Map as Map
import Data.Maybe
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.Word
import Data.Complex
import Control.Monad
import System
import Debug.Trace
import Unsafe.Coerce
import Util

import System.IO.Unsafe

-- README
------------------------------------------------------------
-- How to do a *PURE* CnC?

-- Well, this is a bit tricky because the a cnc step is a function
-- from a heterogeneous set of collections to a set of new tags and
-- items.  We could use various methods:

-- (1) We could require that the user construct a sum-type including
--     all the item types that will occur in their program.  (And
--     another for the tag types.)

-- (2) We could use existential types to pack various sorts of output
--     items and tags into one list.  Together with an unsafe cast we 
--     could build a safe interface into heterogeneous collections.

-- This file implements (2).  This is fairly inefficent because our
-- primary representation is a Map of Maps.  We have the overhead of
-- that double indirection times the cost of the pure data structures.

-- Below you will see two interfaces, the "raw" functional interface
-- (functions prefixed with "_") and a nicer monadic interface.

------------------------------------------------------------
-- Type definitions:

-- The central abstraction is a (heterogeneous) group of item and tag collections.
type Collections = (Int, MatchedTagMap, MatchedItemMap)

data MatchedItemMap = forall a b. MI (IntMap.IntMap (ItemColInternal a b))
data MatchedTagMap  = forall a.   MT (IntMap.IntMap (TagColInternal a))

-- We pass around HANDLES to the real item/tag collections, called "ID"s:
data TagCol  a   = TCID Int deriving (Ord, Eq, Show)
data ItemCol a b = ICID Int deriving (Ord, Eq, Show)
type TagColInternal    a   = Set a
type ItemColInternal   a b = Map a b

-- A step either produces a batch of new data to write, or blocks:
type Step a = a -> Collections -> StepResult
data StepResult = Done [NewTag] [NewItem]
                | forall a b. (Ord a, Show a) => Block (ItemCol a b) a
data NewTag  = forall a.   Ord a => NT (TagCol  a)   a
data NewItem = forall a b. Ord a => NI (ItemCol a b) a b

-- Need it to be a map... but this type is not truly polymorphic enough:
data Graph = forall a. G (IntMap.IntMap [Step a])

------------------------------------------------------------
-- (Optional) type signatures for operations:

-- The raw functional interface:
_newWorld   :: Int -> Collections
_newTagCol  :: Collections -> (TagCol ma, Collections)
_newItemCol :: Collections -> (ItemCol a b, Collections)

-- These are called by the step code and produce outbound tags and items:
_put  :: Ord a => ItemCol a b -> a -> b -> NewItem
_call :: Ord a => TagCol  a   -> a      -> NewTag
_get  :: Ord a => Collections -> ItemCol a b -> a -> Maybe b

_prescribe :: Ord a => TagCol a -> Step a -> Graph -> Graph

--------------------------------------------------------------------------------
-- Implementation:

-- A "world" is a (heterogeneous) set of collections.
-- A world keeps a counter that is used to uniquely name each collection in that world:
_newWorld n = (n, MT IntMap.empty, MI IntMap.empty)
_newTagCol (cnt, MT tags, items) = 
    (TCID cnt, (cnt+1, MT newtags, items))
  where newtags = IntMap.insert cnt Set.empty tags 

_newItemCol (cnt, tags, MI items) =
    (ICID cnt, (cnt+1, tags, MI newitems))
  where newitems = IntMap.insert cnt Map.empty items

magic :: ItemCol a b -> ItemColInternal c d -> ItemColInternal a b
magic id = (unsafeCoerce)

_get (_, _, MI imap) id tag = 
  let ICID n = id 
      badcol  = (IntMap.!) imap n
      goodcol = magic id badcol
   in
   case Map.lookup tag goodcol of
    Nothing -> Nothing
    Just d  -> Just d

_put id tag item = NI id tag item -- Just accumulate puts as data
_call id tag     = NT id tag 

moremagic :: IntMap.IntMap (ItemColInternal a b) -> ItemColInternal c d -> ItemColInternal a b
moremagic id = (unsafeCoerce)

tmagic :: TagCol a -> TagColInternal c -> TagColInternal a
tmagic id = (unsafeCoerce)

mostmagic :: IntMap.IntMap (TagColInternal a) -> TagColInternal c -> TagColInternal a
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
       let items' = foldl (\ acc (NI id k x) -> 
  			    let ICID n = id 
			        badcol = (IntMap.!) acc n
			        goodcol = magic id badcol
 			        newcol = moremagic acc $ Map.insert k x goodcol
			    in
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

megamagic :: TagCol a -> IntMap.IntMap [Step b] -> IntMap.IntMap [Step a]
megamagic id col = (unsafeCoerce col)


emptyGraph = G IntMap.empty
_prescribe id step (G gmap) = 
    case id of 
     TCID n ->
       G (IntMap.insertWith (++) n [step] $ megamagic id gmap)

-- Retrieve the steps from a graph:
getSteps  :: Graph -> TagCol a -> [Step a]
getSteps (G gmap) id = 
    case id of 
     TCID n -> IntMap.findWithDefault [] n (megamagic id gmap)

-- Returns thunks representing the result of the steps:
callSteps  :: Graph -> TagCol a -> a -> [Collections -> StepResult]
callSteps (G gmap) id tag = 
    case id of 
     TCID n -> Prelude.map (\fn -> fn tag) $ 
	       IntMap.findWithDefault [] n (megamagic id gmap)

serialScheduler :: Graph -> [NewTag] -> Collections -> Collections
serialScheduler graph inittags cols = schedloop cols [] inittags []
 where schedloop c [] [] []  = c
       -- FIXME: TEMP HACK -- just try all the blocked ones when we run out of other stuff:
       -- TODO: wake up blocked steps intelligently when the output is produced.
       --schedloop c blocked [] = schedloop c [] blocked
       schedloop c blocked [] [] = error "err ran out of tags but have blocked..."
       schedloop c blocked (hd : tl) [] = 
	   case hd of 
	    NT id tag ->
	     schedloop c blocked tl (callSteps graph id tag)
       schedloop c blocked tags (step:tl) = 
	   --trace (case id of TCID n -> "      *** Executing tagcol "++ show n ++" tag: "++ show (char tag)) $ 
	   case step c of
	     -- FIXME: We don't YET track what item collection we blocked on.
	     Block d_id tag -> trace (" ... Blocked ... ") $
			       schedloop c (step:blocked) tags tl
	     Done newtags newitems -> 
		 schedloop (mergeUpdates newtags newitems c)
		           blocked (newtags++tags) tl
	 -- FIXME: SUPPRESS pre-existing tags. Currently the tag collections do NOTHING


--------------------------------------------------------------------------------
-- Common interface for interoperability:

-- The CncCode monad carries forward a state (newtags, newitems) and
-- blocks on a failed get.
data CncCode a = CC (Collections -> [NewTag] -> [NewItem] -> (Maybe a, StepResult))

-- Currently ONE GRAPH context implicit in the monad (could do many):
-- GraphCode threads through the Collections and Graph values:
data GraphCode a = GC (Collections -> Graph -> [NewTag] -> (Collections, Graph, [NewTag], a))

newTagCol      :: () -> GraphCode (TagCol a)
newItemCol     :: () -> GraphCode (ItemCol a b)

-- The monadic interface 
put  :: Ord a           => ItemCol a b -> a -> b -> CncCode ()
get  :: (Show a, Ord a) => ItemCol a b -> a -> CncCode b
call :: Ord a => TagCol  a   -> a -> CncCode ()

-- CncCode accumulates a list of new items/tags without committing them to the Collections.
instance Monad CncCode where 
    return x = CC$ \w nt ni -> (Just x, Done nt ni)
    (CC ma) >>= f = CC$ \w nt ni -> 
		          case ma w nt ni of 
			   (_, Block ic t) -> (Nothing, Block ic t)
			   (Just a, Done nt' ni') -> let CC mb = f a 
						     in mb w nt' ni'
get col tag = CC $
    \ w tags items -> 
      case _get w col tag of 
       Nothing -> (Nothing, Block col tag)
       Just x  -> (Just x,  Done tags items)

put col tag val = CC $ 
   \ w tags items -> 
      (Just (), Done tags (_put col tag val : items))

call col tag = CC $ 
   \ w tags items -> 
      (Just (), Done (_call col tag : tags) items)


-- The graph monad captures code that builds graphs:
instance Monad GraphCode where 
    return x = GC$ \ w g it -> (w,g,it, x)
    (GC ma) >>= f = 
      GC $ \w g itags -> 
	let (w',g',it',a) = ma w g itags
	    GC mb = f a
	in mb w' g' it'
	
newTagCol () = 
    GC$ \(cnt, MT tags, items) graph inittags -> 
      let newtags = IntMap.insert cnt Set.empty tags in
       ((cnt+1, MT newtags, items), 
        graph, inittags,TCID cnt)

newItemCol () = 
    GC$ \(cnt, tags, MI items) graph inittags -> 
      let newitems = IntMap.insert cnt Map.empty items in
       ((cnt+1, tags, MI newitems), 
        graph, inittags, ICID cnt)

prescribe :: Ord a => TagCol a -> (a -> CncCode ()) -> GraphCode ()
--prescribe tc step = 
prescribe tc stepcode = 
    GC$ \ cols graph inittags -> 
	(cols,
	 _prescribe tc 
	     (\a w -> 
	      let CC fn = stepcode a 
	          (_,result) = fn w [] []
	      in result)
	     graph,
	 inittags, ())    

-- Initialize runs CncCode but does not invoke the scheduler.
-- You should not do any 'gets' inside this CncCode.
-- New tags introduced are accumulated as "inittags":
initialize :: CncCode a -> GraphCode a
initialize (CC fn) = 
    GC$ \w graph inittags -> 
     case fn w inittags [] of 
       -- This commits the new tags/items to the Collections
       (Just x, Done nt ni) ->  
	   --seq (unsafePerformIO $ putStrLn $ show ("  initializing", length nt, length ni)) $
	   (mergeUpdates [] ni w, graph, nt, x)
       (Nothing, Block itemcol tag) ->
	   error ("Tried to run initialization CncCode within the GraphCode monad but it blocked!: "
		 ++ show (itemcol, tag))

-- Execute is like init except that it invokes the scheduler.
-- Any tags already in the collection are taken to be "unexecuted"
-- and make up the inittags argument to the scheduler.
-- 
-- NOTE: The current philosophy is that the serialScheduler runs until
-- nothing is blocking.  Thus the finalize action won't need to block.
-- A different method would be to only run the scheduler just enough
-- to satisfy the finalize action.  That would be nice.
finalize :: CncCode a -> GraphCode a
finalize (CC fn) = 
    GC$ \w graph inittags -> 	
	case w of  
	 (_, MT tmap, _) -> 
	  let finalworld = serialScheduler graph inittags w in
	  -- After the scheduler is done executing, then when run the final action:
          case fn finalworld [] [] of 
	   (Just x, Done [] []) -> (finalworld, graph, [], x)
	   (Just _, Done _ _)   -> error "It isn't proper for a finalize action to produce new tags/items!"
	   (Nothing, Block itemcol tag) ->
	    error ("Tried to run finalization CncCode but it blocked!: "
		 ++ show (itemcol, tag))

runGraph :: GraphCode a -> IO a
runGraph (GC fn) = return x
    where (_,_,_,x) = fn (_newWorld 0) emptyGraph []


gcPrintWorld :: String -> GraphCode ()
gcPrintWorld str =
  GC$ \w g it -> 
   case w of 
    (n, MT tmap, MI imap) ->
     seq (unsafePerformIO $
	  do putStr "GraphCode - Printing world: "
	     putStrLn str
	     putStrLn ("  "++ show (IntMap.size tmap) ++" tag collections "++
		              show (IntMap.size imap) ++" item collections")
	     mapM (\key -> 		    
		    let m = IntMap.findWithDefault (error "shouldn't happen") key tmap in
		    putStrLn ("    Tag col "++ show key ++" size "++ show (Set.size m)))
	       (IntMap.keys tmap)

	     mapM (\key -> 		    
		    let m = IntMap.findWithDefault (error "shouldn't happen") key imap in
		    putStrLn ("    Item col "++ show key ++" size "++ show (Map.size m)))
	       (IntMap.keys imap)
	 )
     (w,g,it,())
       
--   show (n, IntMap.size tmap, IntMap.keys imap, 
-- 	Map.keys foo, 
-- 	Map.elems foo)

gcPutStr :: String -> GraphCode ()
gcPutStr str = 
  GC$ \w g it -> 
     seq (unsafePerformIO (putStr str))
	 (w,g,it,())

--------------------------------------------------------------------------------
-- Test program:

type TI = TagCol  Char
type II = ItemCol Char Int
incrStep :: II -> (TI, II) -> Step Char
incrStep d1 (t2,d2) tag c = 
    case _get c d1 tag of 
      Nothing -> Block d1 tag
      Just n ->  Done [_call t2 tag]
		      [_put  d2 tag (n+1)]

-- Test using the function interface directly:
test = -- Allocate collections:
    let w0      = _newWorld 0
        (t1,w2) = _newTagCol  w0
        (t2,w3) = _newTagCol  w2
        (t3,w4) = _newTagCol  w3
        (d1,w5) = _newItemCol w4
        (d2,w6) = _newItemCol w5
        (d3,w7) = _newItemCol w6
		  
        -- Initialize:
        w8 = mergeUpdates [] [_put d1 'a' 33, 
 			      _put d1 'b' 100] w7

        graph = _prescribe t1 (incrStep d1 (t2,d2)) $
 		_prescribe t2 (incrStep d2 (t3,d3)) $
 		emptyGraph

        inittags = [_call t1 'b', _call t1 'a']

        w9 = serialScheduler graph inittags w8

    in 
     do putStrLn $ showcol w9
        putStrLn $ "  d1: " ++ show (_get w9 d1 'a', _get w9 d1 'b') 
        putStrLn $ "  d2: " ++ show (_get w9 d2 'a', _get w9 d2 'b') 
        putStrLn $ "  d3: " ++ show (_get w9 d3 'a', _get w9 d3 'b') 
        return ()

-- Same test using wrappers:	 
test2 = 
 do v <- runGraph $ do
        t1 <- newTagCol()
        t2 <- newTagCol()
        t3 <- newTagCol()
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

        gcPrintWorld "Initialization finished"

        -- Get some of the results:
        finalize $ do d1a <- get d1 'a'
		      d1b <- get d1 'b'
		      d2a <- get d2 'a'
		      d2b <- get d2 'b'
		      d3a <- get d3 'a'
		      d3b <- get d3 'b'
		      return ((d1a,d1b), (d2a,d2b), (d3a,d3b)) 
    putStrLn ("Final: "++ show v)

showcol (n, MT tmap, MI imap) =
  show (n, IntMap.size tmap, IntMap.keys imap, 
	Map.keys foo, 
	Map.elems foo)
 where 
    -- Hack -- pull out the first item collection:
    foo = (unsafeCoerce $ (IntMap.!) imap 3) :: ItemColInternal Char Int

--instance Show StepResult where
--    show x = "foo"

#include "mandel.hs"

--------------------------------------------------------------------------------
{-
mandel :: Int -> Complex Double -> Int
mandel max_depth c = 
--    trace ("==== MANDEL STEP of " ++ show c) $
    loop 0 0 0
  where   
   fn = magnitude
--   fn = realPart . abs
   loop i z count
    | i == max_depth      = count
    | fn(z) >= 2.0 =  count  -- trace "  FALLOUT2" $
    | otherwise     = --trace (" z parts "++ show (realPart z) ++ " " ++ show (imagPart z)++
		      --       "  looping "++ show i ++ " "++ show count ++" "++ show (fn z)     ) $
		      loop (i+1) (z*z + c) (count+1)

type Pair = (Word16, Word16)

run max_row max_col max_depth = 
    do cref <- newWorld 
       position <- newTagCol  cref  :: IO (TagCol  Pair)
       dat      <- newItemCol cref  :: IO (ItemCol Pair (Complex Double))
       pixel    <- newItemCol cref  :: IO (ItemCol Pair Int)

       let mandelStep tag c =     
	    case _get c dat tag of 
	     Nothing -> trace ("  (blocking mandel step "++ show tag ++")") $ Block dat tag
	     Just cplx ->  --trace ("  (invoking mandel step "++ show cplx ++")") $ 
			Done [] [_put pixel tag (mandel max_depth cplx)]

       let graph = 
	    prescribe position mandelStep $
	    emptyGraph

       let putit :: Word16 -> Word16 -> NewItem
	   putit i j = 
	      _put dat ((i,j) :: Pair) (z :: Complex Double)
	    where 
             z = (r_scale * (fromIntegral j) + r_origin) :+ 
		 (c_scale * (fromIntegral i) + c_origin)
       let inititems = 
	    [ putit i j | i :: Word16 <- [0..max_row-1], j <- [0..max_col-1]]

       let inittags = [ _call position (i,j) 
			| i <- [0..max_row-1], j <- [0..max_col-1]]
       -- Load up init data:
       modifyIORef cref $ mergeUpdates inittags inititems

       -- why do we do this??:
       putStrLn $ show (mandel max_depth (1.0 :+ 1.5))

       modifyIORef cref $ serialScheduler graph inittags

       putStr "Done simple test... Now unpack it:\n"

       c <- readIORef cref
       let foo = _get c dat (0,0) 

--       putStrLn $ "Ok, max_row and max_col: " ++ show (max_row, max_col)

       let check = foldRange 0 max_row 0 $
 		   \ acc i -> foldRange 0 max_col acc $
 		    \ acc j -> 
---		      trace ("Hmm... ij " ++ show (i,j)  ++ 
--			     " max depth "++ show max_depth ++" get pixel: "++ show (get c pixel (i,j))) $
  		      if fromJust (_get c pixel (i,j)) == max_depth
  		      then acc + (i*max_col + j)
  		      else acc
	 
       putStrLn ("Mandel check " ++ show check)
       
       return ()
  where 
   --z = 1.0 :+ 1.5                   :: Complex Double
   r_origin = -2                            :: Double
   r_scale  = 4.0 / (fromIntegral max_row)  :: Double
   c_origin = -2.0                          :: Double
   c_scale = 4.0 / (fromIntegral max_col)   :: Double
-}

--main = do (one : two : three : _) <- getArgs	  
--	  run (read one::Word16) (read two)  (read three)
