{- 
[2004.10.22]

This expands out features of token machines which are essentially macros.  It:

 *)  Reduces "flood" expressions.


-}

module Expand where

import TM

expand_tm (Pgm consts socconsts socpgm nodetoks startup) = 
    let toknames = map (\ (x,_,_) -> x) nodetoks in	
	(Pgm consts socconsts 
	 (map (pe toknames []) socpgm)
	 (map (pth toknames) nodetoks)
	 startup)

-- Process Token Handler
pth tenv (t, ids, bod) = (t, ids, pe tenv [] bod)


-- Process Expression:
pe tenv lenv expr =
    let loop = pe tenv lenv 
    in case expr of
       (Econst c) -> expr 
       (Evar id)  -> expr 
       (Elambda ids e) -> Elambda ids (pe tenv (ids++lenv) e)
       (Elet binds e)  -> 
	   Elet [ (lhs, loop rhs) | lhs <- map fst binds,
		                    rhs <- map snd binds ]
		 (pe tenv (map fst binds ++ lenv) e)

       (Eseq e1 e2) -> Eseq (loop e1) (loop e2)

       (Eif a b c) -> Eif (loop a) (loop b) (loop c)

       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

       (Eprimapp prim args) -> Eprimapp prim (map loop args)
       (Esense) -> Esense

       -- Special forms:
       (Esocreturn e) -> Esocreturn $ loop e
       (Esocfinished) -> Esocfinished
       (Ereturn val to via seed aggr) ->
	   Ereturn (loop val) to via (loop seed) aggr

       (Erelay mbtok) -> Erelay mbtok
       (Eemit mbtime tok args) -> Eemit mbtime tok (map loop args)
       (Ecall mbtime tok args) -> Ecall mbtime tok (map loop args)
       (Eactivate tok args)    -> Eactivate    tok (map loop args)

       (Eflood tok)       -> expr	   
       (Eelectleader tok) -> expr

--       _ -> Econst 999
