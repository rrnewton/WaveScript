{- 
[2004.10.22]

This expands out features of token machines which are essentially macros.  It:

 *)  Reduces "flood" expressions.


-}

module Expand where

import TM
import Utils

import Control.Monad.ST
import Data.STRef
import Data.Char


-----------------------------------------------------------------------------


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

       (Eseq exprs) -> Eseq (map loop exprs)

       (Eif a b c) -> Eif (loop a) (loop b) (loop c)

--       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

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

--init_name_counter :: [String] -> Int
init_name_counter counter many_names = 
    do writeSTRef counter
	 (maximum $
	  map read $
	  filter_just $
	  map (largest_contig isDigit) 
              many_names)
    

--unique_name :: STRef s a -> [Char] -> ST s [Char]
unique_name counter root = 
    do count <- readSTRef counter
       return (root ++ "_" ++ show count)

-- Returns: a new replacement expression and a list of new token-handlers.
expand_macros :: [Token] -> [Id] -> Expr -> (Expr, [TokHandler])
expand_macros tenv lenv expr = 
    runST 
     (do new_tokhands <- newSTRef []       
	 name_counter <- newSTRef 0       
	 init_name_counter name_counter 
           (map (\ (Token s) -> s) tenv ++
	    map (\ (Id s) -> s) lenv)
	 let newname = unique_name name_counter
	 let loop lenv expr = case expr of

	   -- For now can only flood tokens with no args!
               (Eflood tok ) -> 
		   do tokhands <- readSTRef new_tokhands
		      name <- newname "spine"
		      writeSTRef new_tokhands
				 (( Token name, [], 
				    Eseq [(Erelay Nothing),
					  (Ecall Nothing tok [])]) 
				  : tokhands)
		      return (Eemit Nothing (Token name) [])
	       (Eelectleader tok) -> return expr

               (Econst c) -> return expr
	       (Evar id)  -> return expr

--	       (Elambda ids e) -> Elambda ids (pe tenv (ids++lenv) e)
	       (Elet binds e)  -> 
		   do let lhss = map fst binds
		      rhss <- mapM (loop lenv) (map snd binds)
		      bod <- loop (lhss ++ lenv) e
		      return (Elet (zip lhss rhss) bod)

	       (Eseq es) -> do new_es <- mapM (loop lenv) es
			       return (Eseq new_es)
{-	       (Eif a b c) -> Eif (loopsame a) (loop b) (loop c)


--       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

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
-}
	 newexpr <- loop lenv expr
	 newtoks <- readSTRef new_tokhands
	 return (newexpr,newtoks)
     )
		    

{-       (Elambda ids e) -> return $ Elambda ids (pe tenv (ids++lenv) e)
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
-}




{-
       return (loop expr)
    where 
    loop e =
    case expr -}