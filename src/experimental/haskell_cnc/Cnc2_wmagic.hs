{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
  #-}
{-
   , PatternSignatures
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

data MatchedItemMap = forall a b. MI (IntMap.IntMap (ItemCol a b))
data MatchedTagMap  = forall a.   MT (IntMap.IntMap  (TagCol a))

-- We pass around HANDLES to the real item/tag collections, called "ID"s:
data TagColID  a   = TCID Int deriving (Ord, Eq)
data ItemColID a b = ICID Int
type TagCol    a   = Set a
type ItemCol   a b = Map a b

-- A step either produces a batch of new data to write, or blocks:
type Step a = a -> Collections -> StepResult
data StepResult = Done [NewTag] [NewItem]
                | forall a b. Ord a => Block (ItemColID a b) a
data NewTag  = forall a.   Ord a => NT (TagColID  a)   a
data NewItem = forall a b. Ord a => NI (ItemColID a b) a b

-- Need it to be a map... but this type is not truly polymorphic enough:
data Graph = forall a. G (IntMap.IntMap [Step a])

------------------------------------------------------------
-- (Optional) type signatures for operations:

-- The raw functional interface:
_newWorld   :: Int -> Collections
_newTagCol  :: Collections -> (TagColID a, Collections)
_newItemCol :: Collections -> (ItemColID a b, Collections)

newWorld   :: IO (IORef Collections)
newTagCol      :: IORef Collections -> IO (TagColID a)
newItemCol     :: IORef Collections -> IO (ItemColID a b)
-- These are called by the step code and produce outbound tags and items:
_put  :: Ord a => ItemColID a b -> a -> b -> NewItem
_call :: Ord a => TagColID  a   -> a      -> NewTag
_get  :: Ord a => Collections -> ItemColID a b -> a -> Maybe b


-- The monadic interface 
put  :: Ord a => ItemColID a b -> a -> b -> CncCode ()
get  :: Ord a => ItemColID a b -> a -> CncCode b
call :: Ord a => TagColID  a   -> a -> CncCode ()

--------------------------------------------------------------------------------
-- Implementation:


_newWorld n = (n, MT IntMap.empty, MI IntMap.empty)
_newTagCol (cnt, MT tags, items) = 
    (TCID cnt, (cnt+1, MT newtags, items))
  where newtags = IntMap.insert cnt Set.empty tags 

_newItemCol (cnt, tags, MI items) =
    (ICID cnt, (cnt+1, tags, MI newitems))
  where newitems = IntMap.insert cnt Map.empty items

magic :: ItemColID a b -> ItemCol c d -> ItemCol a b
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

megamagic :: TagColID a -> IntMap.IntMap [Step b] -> IntMap.IntMap [Step a]
megamagic id col = (unsafeCoerce col)


emptyGraph = G IntMap.empty
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

instance Monad CncCode where 
    return x = CC$ \w nt ni -> (Just x, Done [] [])
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


-- The graph monad for captures code that builds graphs:


-- A "world" is a (heterogeneous) set of collections.
-- A world keeps a counter that is used to uniquely name each collection in that world:
newWorld = newIORef (0, MT IntMap.empty, MI IntMap.empty)
newTagCol ref = do (cnt, MT tags, items) <- readIORef ref		   
		   let newtags = IntMap.insert cnt Set.empty tags
		   writeIORef ref (cnt+1, MT newtags, items)
		   return (TCID cnt)
newItemCol ref = do (cnt, tags, MI items) <- readIORef ref 	   
		    let newitems = IntMap.insert cnt Map.empty items
		    writeIORef ref (cnt+1, tags, MI newitems)
		    return (ICID cnt)



--------------------------------------------------------------------------------
-- Test program:

type TI = TagColID  Char
type II = ItemColID Char Int
incrStep :: II -> (TI, II) -> Step Char
incrStep d1 (t2,d2) tag c = 
    case _get c d1 tag of 
      Nothing -> Block d1 tag
      Just n ->  Done [_call t2 tag]
		      [_put  d2 tag (n+1)]

-- Test using the pure interface directly:
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

        graph = prescribe t1 (incrStep d1 (t2,d2)) $
 		prescribe t2 (incrStep d2 (t3,d3)) $
 		emptyGraph

        inittags = [_call t1 'b', _call t1 'a']

        w9 = serialScheduler graph inittags w8

    in 
     do putStrLn $ showcol w9
        putStrLn $ "  d1: " ++ show (_get w9 d1 'a', _get w9 d1 'b') 
        putStrLn $ "  d2: " ++ show (_get w9 d2 'a', _get w9 d2 'b') 
        putStrLn $ "  d3: " ++ show (_get w9 d3 'a', _get w9 d3 'b') 
        return ()

	 

showcol (n, MT tmap, MI imap) =
  show (n, IntMap.size tmap, IntMap.keys imap, 
	Map.keys foo, 
	Map.elems foo)
 where 
    -- Hack -- pull out the first item collection:
    foo = (unsafeCoerce $ (IntMap.!) imap 3) :: ItemCol Char Int


--------------------------------------------------------------------------------

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
       position <- newTagCol  cref  :: IO (TagColID  Pair)
       dat      <- newItemCol cref  :: IO (ItemColID Pair (Complex Double))
       pixel    <- newItemCol cref  :: IO (ItemColID Pair Int)

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
--   verbose = false
--     int *pixels = new int[max_row*max_col];

--     double r_origin = -2;
--     double r_scale = 4.0/max_row;
--     double c_origin = -2.0;
--     double c_scale = 4.0/max_col;
--     int *pixels = new int[max_row*max_col];



--main = run (3::Word16) (3::Word16) 3
--m = run (2::Word16) (2::Word16) 2

main = do (one : two : three : _) <- getArgs	  
	  run (read one::Word16) (read two)  (read three)


-- Read max_rows...
--         fprintf(stderr,"Usage: mandel [-v] rows columns max_depth\n");
--         max_row = atoi(argv[ai]);
--         max_col = atoi(argv[ai+1]);
--         max_depth = atoi(argv[ai+2]);


--     c.wait();
    
--     for (int i = 0; i < max_row; i++) 
--     {
--         for (int j = 0; j < max_col; j++ )
--         {
--             pixels[i*max_col + j] = c.m_pixel.get(make_pair(i,j));
--         }
--     }
    
--     tbb::tick_count t1 = tbb::tick_count::now();
--     printf("Mandel %d %d %d in %g seconds\n", max_row, max_col, max_depth,
--            (t1-t0).seconds());
    
--     int check = 0;
--     for (int i = 0; i < max_row; i++) 
--     {
--         for (int j = 0; j < max_col; j++ )
--         {
--             if (pixels[i*max_col + j ] == max_depth) check += (i*max_col +j );
--         }
--     }
--     printf("Mandel check %d \n", check);
    
--     if (verbose)
--     {
--         for (int i = 0; i < max_row; i++) 
--         {
--             for (int j = 0; j < max_col; j++ )
--             {
--                 if (pixels[i*max_col + j] == max_depth)
--                 {
--                     std::cout << " ";
--                 }
--                 else
--                 {
--                     std::cout << ".";
--                 }
--             }
--             std::cout << std::endl;
--         }
--     }
--     delete [] pixels;
-- }
