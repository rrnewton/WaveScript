{-  [2004.10.28]
This simply flattens out the token machine into single assignment form.
-}

module Flatten where

import Utils
import TM
import TM_simple as TMS 

import Control.Monad.State

{-newname :: String -> State Int String
newname s = do counter <- get
	       set (counter + 1)
	       return (s ++ show counter)
-}

flatten_tm :: TMPgm -> State Int TMS.Pgm
flatten_tm (TM.Pgm consts socconsts socpgm nodetoks startup) =     
--    let toknames = map (\ ((Token x), _,_) -> x) nodetoks in
    do socblock <- foldM (\ blk e -> do newblk <- pe e
			                return (append_blocks blk newblk))
		         (Block [] []) socpgm				       
       nodetoks2 <- foldM (\ acc th -> do newtokh <- pth th
			                  return [newtokh] )--(newtokh : acc))
		          [] nodetoks
       return (TMS.Pgm consts socconsts socblock nodetoks2 startup)

-- Process TokenHandler
pth :: TM.TokHandler -> State Int TMS.TokHandler
pth (t,ids,e) = do e' <- pe e
		   return (t,ids,e')

-- Process Expression
pe :: Expr -> State Int Block
pe e = 
    case e of 
       (Econst c) -> return (Block [] [])
       (Evar id)  -> return (Block [] [])
--       (Elambda ids e) -> error "Flatten.pe: should not have Elambda at this point"

{-       (Elet binds e)  -> 
	   Elet [ (lhs, pe rhs) | lhs <- map fst binds,
		                    rhs <- map snd binds ]
		 (pe (map fst  e)
-}

--       (Eseq exprs) -> Eseq (map pe exprs)

{-       (Eif a b c) -> Eif (pe a) (pe b) (pe c)

--       (Eprimapp Pflood tok) -> Eprimapp prim (map pe args)

       (Eprimapp prim args) -> Eprimapp prim (map pe args)
       (Esense) -> Esense

       -- Special forms:
       (Esocreturn e) -> Esocreturn $ pe e
       (Esocfinished) -> Esocfinished
       (Ereturn val to via seed aggr) ->
	   Ereturn (pe val) to via (pe seed) aggr

       (Erelay mbtok) -> Erelay mbtok
       (Eemit mbtime tok args) -> Eemit mbtime tok (map pe args)
       (Ecall mbtime tok args) -> Ecall mbtime tok (map pe args)
       (Eactivate tok args)    -> Eactivate    tok (map pe args)

       (Eflood tok)       -> e	   
       (Eelectleader tok) -> e

-}

bottom = bottom

----------------------------------------------------------------------
-- HELPERS:

append_blocks :: Block -> Block -> Block
append_blocks (Block binds1 stmts1) (Block binds2 stmts2) =
    Block (binds1 ++ binds2) (stmts1 ++ stmts2)
