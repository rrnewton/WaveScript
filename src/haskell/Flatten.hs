{-  [2004.10.28]
This simply flattens out the token machine into single assignment form.
-}

module Flatten where

import Utils
import TM
import TM_simple as TMS 

import Data.Set
import Control.Monad.State

flatten_tm :: TMPgm -> State Int TMS.Pgm
flatten_tm (TM.Pgm consts socconsts socpgm nodetoks startup) =     
--    let toknames = map (\ ((Token x), _,_) -> x) nodetoks in
    do socblock <- foldM (\ blk e -> do (newblk,_) <- pe e
			                return (append_blocks blk newblk))
		         (Block [] []) socpgm				       
       nodetoks2 <- foldM (\ acc th -> do newtokh <- pth th
			                  return [newtokh] )--(newtokh : acc))
		          [] nodetoks
       return (TMS.Pgm consts socconsts socblock nodetoks2 startup)

-- Process TokenHandler
pth :: TM.TokHandler -> State Int TMS.TokHandler
pth (t,ids,e) = do (e',_) <- pe e
		   return (t,ids,e')

-- Process Expression - Returns a block and a basic expression for the return value.
pe :: Expr -> State Int (Block, Maybe Basic)
pe e = do (b,rv) <- loop e 
          return (preen_block b, rv)
    where 
    loop e = 
	case e of 
       (Econst c) -> return (Block [] [], Just (Bconst c))
       (Evar id)  -> return (Block [] [], Just (Bvar id))
--       (Elambda ids e) -> error "Flatten.pe: should not have Elambda at this point"

       (Elet binds e)  -> 
	   do (blk1, ret) <- loop e
	      blks <- foldM (\ bs (v,bod) ->
			     do (Block vars stmts, Just r) <- loop bod
			        let b = Block ([v]++vars)
			                      (stmts++[Sassign v r])
			        return (b:bs))
		      [] binds
	      return (concat_blocks (blk1 : reverse blks),
		      ret)

       (Eseq exprs) -> do (blks, ret) <- foldM (\ (bs,_) e ->
						do (b,r) <- loop e;
						   return (b:bs, r))
					 ([],Nothing) exprs
			  return (concat_blocks (reverse blks), ret)

       (Eif a b c) -> do newvar <- newid "ifret_" 
			 (blka,Just ra) <- loop a
			 (blkb,rb) <- loop b
			 (blkc,rc) <- loop c			 
			 let build_assign (Just r) = [Sassign newvar r]
			     build_assign Nothing  = []
			 return (Block (newvar : binds blka ++ binds blkb ++ binds blkc)
				       (stmts blka ++
					[(Sif ra
					  (Block [] (stmts blkb ++ build_assign rb)) 
					  (Block [] (stmts blkc ++ build_assign rc)))]),
				 Bvar newvar)
				 

--       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

{-
       (Eprimapp prim args) -> Eprimapp prim (map loop args)
       (Esense) -> Esense

       -- Sloopcial forms:
       (Esocreturn e) -> Esocreturn $ loop e
       (Esocfinished) -> Esocfinished
       (Ereturn val to via seed aggr) ->
	   Ereturn (loop val) to via (loop seed) aggr

       (Erelay mbtok) -> Erelay mbtok
       (Eemit mbtime tok args) -> Eemit mbtime tok (map loop args)
       (Ecall mbtime tok args) -> Ecall mbtime tok (map loop args)
       (Eactivate tok args)    -> Eactivate    tok (map loop args)

       (Eflood tok)       -> e	   
       (Eelectleader tok) -> e

-}

bottom = bottom
test = evalState (flatten_tm a) 

----------------------------------------------------------------------
-- HELLOOPRS:

newid :: String -> State Int Id
newid = do counter <- get
	   put (counter + 1)
	   return (Id (s ++ show counter))

append_blocks :: Block -> Block -> Block
append_blocks (Block binds1 stmts1) (Block binds2 stmts2) =
    Block (binds1 ++ binds2) (stmts1 ++ stmts2)


concat_blocks :: [Block] -> Block
concat_blocks blks =
    Block (concat $ map binds blks) 
	  (concat $ map stmts blks)

-- This just makes the list of used vars a set. 
preen_block :: Block -> Block
--preen_block (Block vars s, rv) = (Block (toSet vars) s, rv)
preen_block (Block vars s, rv) = (Block vars s, rv)

--toSet = setToList . mkSet
