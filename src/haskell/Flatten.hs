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
       let convertc = map (\ (id, Econst n) -> (id, Bconst n))
		      
       return (TMS.Pgm (convertc consts) (convertc socconsts) socblock nodetoks2 startup)


-- Process TokenHandler
pth :: TM.TokHandler -> State Int TMS.TokHandler
pth (t,ids,e) = do (blk, Just r) <- pe e			      
		   return (t,ids,
			   append_blocks blk 
			   (Block [] [Sreturn r]))

-- Process Expression - Returns a block and a basic expression for the return value.
pe :: Expr -> State Int (Block, Maybe Basic)
pe e = do (b,rv) <- loop e 
          return (preen_block b, rv)
	  -- ^^ This is silly in retrospect I should have counted up the free-vars at the end.
    where 
    loop :: Expr -> State Int (Block, Maybe Basic)
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
			 let varbinds = (newvar : binds blka ++ binds blkb ++ binds blkc)
			 let build_assign (Just r) = [Sassign newvar r]
			     build_assign Nothing  = []
			 let statements = (stmts blka ++
					   [(Sif ra
					     (stmts blkb ++ build_assign rb)
					     (stmts blkc ++ build_assign rc))])
			 return (Block varbinds statements, Just $ Bvar newvar)

       (Eprimapp prim args) -> do args' <- mapM loop args
				  id <- newid "pres_"
				  return (gencode id prim args', 
					  Just $ Bvar id)


       (Esense) -> do newvar <- newid "sens_" 
		      return (Block [newvar] [Ssense (Just newvar)],
			      Just $ Bvar newvar)

       -- Sloopcial forms:
       (Esocreturn e) -> do (blk, Just ret) <- loop e
			    return (append_blocks blk (Block [] [Ssocreturn ret]),
				    Nothing)
       (Esocfinished) -> return (Block [] [Ssocfinished], Nothing)
       (Ereturn val to via seed aggr) ->
	   do (blk1,Just rval)  <- loop val
	      (blk2,Just rseed) <- loop seed
	      -- Maybe this should return the local vars used, but it doesn't matter:
	      return (concat_blocks [blk1, blk2,
				     Block [] [Sgradreturn rval to via rseed aggr]],				     
		      Nothing)

       (Erelay mbtok) -> return (Block [] [Srelay mbtok], Nothing)

       (Eemit mbtime tok args) -> 
	   do args' <- mapM loop (args::[Expr])
	      let rets = map (\ (_, Just r) -> r) args'
	      return (concat_blocks (Block [] [Semit mbtime tok rets]
				     : map fst args'),
		      Nothing)
{-
       (Ecall mbtime tok args) ->
	   do args' <- mapM loop args
	      return (concat_blocks (Block [] [Scall mbtime tok (map snd args)]
				     : map fst args),
		      Nothing)
       (Eactivate tok args)    -> 
	   do args' <- mapM loop args
	      return (concat_blocks (Block [] [Sactivate tok (map snd args)]
				     : map fst args),
		      Nothing)
-}
       (Eflood tok)       -> error "Flatten.hs cannot handle flood expressions!"
       (Eelectleader tok) -> error "Flatten.hs cannot handle election expressions!"


-- This really doesn't do much right now.  Theoretically it could do
-- elaborate primitive-specific things:
gencode :: Id -> TM.Prim -> [(Block,Maybe Basic)] -> Block
gencode id p args = 
    append_blocks (concat_blocks $ map fst args)
		  (Block [id] [ Sprimapp (Just id) p (map (\ (_, Just b) -> b) args)])


bottom = bottom
--test = evalState (flatten_tm a) 

----------------------------------------------------------------------
-- HELLOOPRS:

newid :: String -> State Int Id
newid s = do counter <- get
	     put (counter + 1)
	     return (Id (s ++ show counter))

append_blocks :: Block -> Block -> Block
append_blocks (Block binds1 stmts1) (Block binds2 stmts2) =
    Block (binds1 ++ binds2) (stmts1 ++ stmts2)


concat_blocks :: [Block] -> Block
concat_blocks blks =
    Block (concat $ map binds blks) 
	  (concat $ map stmts blks)

-- FIXME #FIXME <TODO> 
-- #Fixme
-- This just makes the list of used vars a set. 
preen_block :: Block -> Block
--preen_block (Block vars s, rv) = (Block (toSet vars) s, rv)
preen_block (Block vars s) = (Block vars s)

--toSet = setToList . mkSet
