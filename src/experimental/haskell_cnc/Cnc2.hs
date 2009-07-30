{-# OPTIONS -fglasgow-exts #-}
{-
  This is the second version.

  Here the primary goal is to make the steps themselves pure functions.  

  A step returns a list of output tags and a list of output items.

  We perform some existential type tricks to allow this polymorphic
  list output (mixing several types together).

 -}


import Data.Set as Set
import Data.Map as Map
--import Data.IntMap as Map
import Data.IORef
import Control.Monad.ST
import Control.Concurrent
--import System.IO.Unsafe

------------------------------------------------------------
-- Type definitions:

-- We could use various methods

-- (1) We could require that the user construct a sum-type including
--     all the item types that will occur in their program.  (And
--     another for the tag types.)

-- (2) We could use existential types to pack various sorts of output
--     items and tags into one list.  But then we could only act on
--     them with the methods of a particular type class.  That would
--     enable extracting a tag and sorting them into their respective
--     collections.  But even once they lived in those collections,
--     there would be no way to recover the typeinformation and they
--     would live on only as that universal type.  (How can they be
--     consumed by the downstream??)  This might be a place for an
--     unsafe cast if it exists....

type Step intag outtag item = intag -> ([outtag], [item])

--type TagCol a = MVar (Set (MVar a))
type TagCol  a   = Set a
type ItemCol a b = MVar (Map a (MVar b))

--class Tag a where 
--  getUniqueIndex :: a -> Int

-- Yuck, we don't want to do this:
--type Step1 a b c     = a -> (Set b, Map b c)
--type Step2 a b c d e = a -> (Set b, Map b c, Set d, Map d e)


-- (3) Don't forget that we can glean type information from the item
--     collections that we consume!  That's where the type info comes
--     from, and we need to encode that in our representation somehow.
--     But how then to use it when we need to unpack one of our
--     outbound values?  How similar is this to the problem faced when
--     implementing IORefs or STRefs?  




------------------------------------------------------------
-- (Optional) type signatures for operations:

-- You can mostly ignore 's' type variables.
newTagCol  :: TagCol a
newItemCol :: IO (ItemCol a b)
-- call :: Ord a           => TagCol  s a   -> a      -> CncCode s ()
-- put  :: Ord a           => ItemCol s a b -> a -> b -> CncCode s ()
-- get  :: (Ord a, Show a) => ItemCol s a b -> a      -> CncCode s b

-- newGraph  :: CncCode s (Graph s)
-- prescribe :: Graph s -> MatchedPair s -> CncCode s ()

--------------------------------------------------------------------------------
-- Implementation:

newTagCol  = Set.empty

newItemCol = do (newMvar Map.empty)

-- call col tag = 
--     do (id, frontset,backset) <- readSTRef col
--        writeSTRef col (id, Set.insert tag frontset, backset)

-- put col tag item = 
--     do map <- readSTRef col
--        writeSTRef col (Map.insert tag item map)

-- get col tag = 
--     do map <- readSTRef col
--        return $ Map.findWithDefault 
-- 		  (error ("get item: tag not found " ++ show tag)) tag map

-- newGraph = do ref <- newSTRef (Map.empty :: Map TagColID [MatchedPair s])
-- 	      return (Graph ref)

-- prescribe (Graph graph) (MP (tagc,step)) = 
--     do stepmap <- readSTRef graph
--        (id,frnt,bck) <- readSTRef tagc
--        let ls = Map.findWithDefault ([]::[MatchedPair s]) id stepmap 
--        let pr = MP (tagc,step) 
--        writeSTRef graph (Map.insert id (pr:ls) stepmap)
--        return ()

-- runSimpleScheduler (Graph graph) = 
--     do    stepmap <- readSTRef graph
-- 	  unsafeIOToST $ putStr ("Size of map " ++ show (Map.size stepmap) ++"\n")
-- 	  allempty <-
-- 	   Map.fold (\ ls acc ->   -- For each tag collection:
-- 		    case head ls of
-- 		     (MP (tags,_)) -> 
-- 		        do (id,frnt,bck) <- readSTRef tags
-- 		           bool <- acc
-- 		           let newbool = bool && Set.null frnt
-- 		           -- For each step prescribed by this tag collection:
-- 		           foldr (\ (MP (tags,step)) (acc) ->
-- 				  do (id,frnt,bck) <- readSTRef tags
-- 				     -- Invoke the step:
-- 				     Set.fold (\ tag acc -> do acc; step tag)
-- 				              acc frnt)
-- 		                 (return ()) ls
-- 		           -- After we're done consuming the fresh tags, clear them:
-- 		           writeSTRef tags (id, Set.empty, Set.union frnt bck)
-- 		           unsafeIOToST $ putStr ("Finished tag collection " ++ show id ++"\n")
-- 		           return newbool)
-- 		   (return True)
--          	   stepmap	  
-- 	  if allempty
-- 	   then unsafeIOToST $ putStr ("All tag collection inboxes empty.  Finished.\n")
-- 	   else runSimpleScheduler (Graph graph)


-- --------------------------------------------------------------------------------
-- -- Test program:

-- Here's an example of what it would be like to lift all tags/items into one type manually:
data MyTags  = Tag1 Int
data MyItems = Item1 Int Int
          -- | Item2 Float ...

-- And we can extract a tag to route items to the right collection:
route (Item1 _ _) = 0
--route (Item2 _) = 1
routetag (Tag1 _) = 0

isPrime 2 = True
isPrime n = (loop 3 == n)
    where loop i = if (n `rem` i) == 0
		   then i else loop (i+2)

-- Here's an example step:
step t =  ([], if isPrime t then [Item1 t t] else [])

-- primes n = 
--    do primes <- newItemCol() :: IO (ItemCol Int Int)
--       put primes 2 2
--       tags <- newTagCol()
--       prescribe tags (\t -> if isPrime t 
-- 		            then put primes t t
-- 		            else return ())
--       let loop i | i >= n = return ()
--   	  loop i = do call tags i 
-- 	              loop (i+2)
--       loop 3
--       result <- itemsToList primes
--       return (take 30 (Prelude.map fst result))
	       


-- incrStep d1 (t2,d2) tag = 
--  do val <- get d1 tag 
--     unsafeIOToST $ putStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
--     put d2 tag (val + 1)
--     call t2 tag

-- test = -- Allocate collections:
--     do t1 <- newTagCol 0
--        t2 <- newTagCol 1
--        t3 <- newTagCol 2
--        d1 <- newItemCol
--        d2 <- newItemCol
--        d3 <- newItemCol
--        -- Initialize:
--        put d1 'a' 33
--        put d1 'b' 100
--        call t1 'a'
--        call t1 'b'
--        -- Build and execute the graph:
--        graph <- newGraph
--        prescribe graph (MP (t1, incrStep d1 (t2,d2)))		 
--        prescribe graph (MP (t2, incrStep d2 (t3,d3)))
--        runSimpleScheduler graph
--        result1 <- get d3 'a'
--        result2 <- get d3 'b'
--        return (result1, result2) 


-- go = runST test


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

