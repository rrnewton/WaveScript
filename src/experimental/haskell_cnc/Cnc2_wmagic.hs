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
import Unsafe.Coerce
import Control.Monad
import qualified Data.IntMap as IntMap
import Debug.Trace

--import Data.Int
import Data.Word

import Data.Complex

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
data StepResult = Done [NewTag] [NewItem]
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
-- Test program:

type TI = TagColID  Char
type II = ItemColID Char Int
incrStep :: II -> (TI, II) -> Step Char
incrStep d1 (t2,d2) tag c = 
    case get c d1 tag of 
      Nothing -> Block d1 tag
      Just n ->  Done [call t2 tag]
		      [put  d2 tag (n+1)]

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
       let inittags = [call t1 'b', call t1 'a']

       putStrLn "About to start scheduler...\n"
       modifyIORef cref $ serialScheduler graph inittags

       c  <- readIORef cref
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



main = test



--------------------------------------------------------------------------------

--instance Monad (StepResult

--mget 
--mput

--------------------------------------------------------------------------------



--max_depth = 10


mandel :: Int -> Complex Double -> Int
mandel max_depth c = 
    trace ("==== MANDEL STEP of " ++ show c) $
    loop 0 0 0
  where   
   fn = magnitude
--   fn = realPart . abs
   loop i z count
    | i == max_depth      = count
    | fn(z) >= 2.0 = trace "  FALLOUT2" $ count 
    | otherwise     = trace (" z parts "++ show (realPart z) ++ " " ++ show (imagPart z)++
			     "  looping "++ show i ++ " "++ show count ++" "++ show (fn z) 
			     ) $
		      loop (i+1) (z*z + c) (count+1)

-- int mandel(const complex &c)
-- {
--     int count = 0;
--     complex z = 0;

--     for(int i = 0; i < max_depth; i++)
--     {
--         if (abs(z) >= 2.0) break;
--         z = z*z + c;
--         count++;
--     }
--     return count;
-- }


type Pair = (Word16, Word16)

run max_row max_col max_depth = 
    do cref <- newCollections 
       position <- newTagCol  cref  :: IO (TagColID  Pair)
       dat      <- newItemCol cref  :: IO (ItemColID Pair (Complex Double))
       pixel    <- newItemCol cref  :: IO (ItemColID Pair Int)

       let mandelStep tag c =     
	    case get c dat tag of 
	     Nothing -> trace ("  (blocking mandel step "++ show tag ++")") $ Block dat tag
	     Just n ->  trace ("  (invoking mandel step "++ show n ++")") $ 
			Done [] [put pixel tag (mandel max_depth n)]

       let graph = 
	    prescribe position mandelStep $
	    emptyGraph

       let putit :: Word16 -> Word16 -> NewItem
	   putit i j = 
	      put dat ((i,j) :: Pair) (z :: Complex Double)
	    where 
             z = (r_scale * (fromIntegral j) + r_origin) :+ 
		 (c_scale * (fromIntegral i) + c_origin)
       let inititems = 
	    [ putit i j | i :: Word16 <- [0..max_row-1], j <- [0..max_col-1]]

       let inittags = [ call position (i,j) 
			| i <- [0..max_row-1], j <- [0..max_col-1]]
       -- Load up init data:
       modifyIORef cref $ mergeUpdates inittags inititems

       -- why do we do this??:
--       putStrLn $ show (mandel max_depth (1.0 :+ 1.5))

       modifyIORef cref $ serialScheduler graph inittags

       putStr "Done simple test... Now unpack it:\n"

       c <- readIORef cref
       let foo = get c dat (0,0) 

       let check = foldRange 0 (max_row-1) 0 $
 		   \ acc i -> foldRange 0 (max_col-1) acc $
 		    \ acc j -> 
--		      trace ("Hmm... ij " ++ show i ++ " " ++ show j ++ 
--			     " max depth "++ show max_depth ++" "++ show (get c dat (i,j))) $
  		      if fromJust (get c pixel (i,j)) == max_depth
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


m = run (3::Word16) (3::Word16) 3


-- Start = inclusive, End = uninclusive:
foldRange start end acc fn = loop start acc
 where
  loop !start !acc
    | start == end = acc
    | otherwise = loop (start+1) (fn acc start)



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
