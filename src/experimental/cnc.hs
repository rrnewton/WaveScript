{-
  This first version is serial.
  It uses a single state thread to track all the tag/item collection state.
 -}

import Control.Monad.ST
import Data.STRef
import  Data.Set as Set
import  Data.Map as Map

import System.IO.Unsafe

type Dummy = ()

-- A tag collection has a buffer of newly written tags.
-- It also has a unique identifier.
type TagColID = Int
type TagCol  s a   = STRef s (TagColID, Set a, Set a)

type ItemCol s a b = STRef s (Map a b)
type Step s a = a -> CncCode s ()

-- The monad stores all the information in the tag/item collections
--
-- It's a state thread.  There only needs to be ONE state thread.  So
-- I think that means I can use something degenerate for the
-- state-thread-distinguishing type arguument.
type CncCode s a = ST s a

--instance Monad CncCode where
--    Hmm [a] >>= f  =  f a
--    return x = Hmm [x]

counter = newSTRef 0

newTagCol :: TagColID -> CncCode s (TagCol s a)
newTagCol cnt = 
            do --tmp <- counter 
	       --cnt <- readSTRef tmp
	       --writeSTRef tmp (cnt+1)
	       newSTRef (cnt+1, Set.empty,Set.empty)

newItemCol :: CncCode s (ItemCol s a b)
newItemCol = newSTRef Map.empty

call :: Ord a => TagCol s a -> a -> CncCode s ()
call col tag = 
    do (id, frontset,backset) <- readSTRef col
       writeSTRef col (id, Set.insert tag frontset, backset)


put :: Ord a => ItemCol s a b -> a -> b -> CncCode s ()
put col tag item = 
    do map <- readSTRef col
       writeSTRef col (Map.insert tag item map)

get :: Ord a => ItemCol s a b -> a      -> CncCode s b
get col tag = 
    do map <- readSTRef col
       return $ Map.findWithDefault undefined tag map


-- Tricky existential types:
-- Here's a tag collection and associated step that "match" tag-types:
data MatchedPair s = forall t. Ord t => MP (TagCol s t, Step s t)

data Graph s = Graph (STRef s (Map TagColID [MatchedPair s]))

-- Do we need a different monad for wiring together the graph??
-- Not in this version...

-- A graph data structure keeps track of the steps that are prescribed
-- everything that is needed by the scheduler.
newGraph :: CncCode s (Graph s)
newGraph = do ref <- newSTRef (Map.empty :: Map TagColID [MatchedPair s])
	      return (Graph ref)

prescribe :: Graph s -> MatchedPair s -> CncCode s ()
prescribe (Graph graph) (MP (tagc,step)) = 
    do stepmap <- readSTRef graph
       (id,frnt,bck) <- readSTRef tagc
       let ls = Map.findWithDefault ([]::[MatchedPair s]) id stepmap 
       let pr = MP (tagc,step) 
       writeSTRef graph (Map.insert id (pr:ls) stepmap)
       return ()

runSimpleScheduler (Graph graph) = 
    do stepmap <- readSTRef graph
       let doEach _ acc = acc
       let doList (MP (tags,step)) acc = 
	     do (id,frnt,bck) <- readSTRef tags 
		val <- acc
	        return (if Set.null frnt
			then val 
			else val)
       let foo = Map.fold (\ ls acc -> foldr doList acc ls)
		          (return 99)
         		  stepmap
       return ()


-- System.IO.Unsafe.unsafePerformIO


test = 
    do t1 <- newTagCol 0
       t2 <- newTagCol 1
       d1 <- newItemCol
       d2 <- newItemCol
       -- Initialize:
       put d1 'a' 33
       call t1 'a'
       let step1 t = 
	       do val <- get d1 t 
		  put d2 t (val + 1)
		  call t2 t
       graph <- newGraph
       prescribe graph (MP (t1,step1))		 
       runSimpleScheduler graph
       result <- get d2 'a'
       return result 


go = runST test


--type T = Int
--type V = String
--type Kern a b = (T,[V]) -> ([T],[V])


{- 

ghci -XRankNTypes cnc.hs
ghci -fglasgow-exts cnc.hs

-}