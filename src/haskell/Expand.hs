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

expand_tm :: TMPgm -> TMPgm
expand_tm (Pgm consts socconsts socpgm nodetoks startup) = 
    let toknames = map (\ (x,_,_) -> x) nodetoks 
	temp = map (expand_macros Nothing toknames []) socpgm
	newsoc = map fst temp
	newths = concat $ map snd temp
    in
	(Pgm consts socconsts 
	 newsoc
	 (pths toknames nodetoks ++ newths)
	 startup)


-- Process Token Handlers
pths :: [Token] -> [TokHandler] -> [TokHandler]
pths tenv ths = 
    foldl (\ acc (t, ids, bod) -> 
	   let (e, newths) = expand_macros (Just t) tenv [] bod
	   in (t, ids, e) : (newths ++ acc))
    [] ths
{-
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
       (ELed c a) -> ELed c a 

--       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

       (Eprimapp prim args) -> Eprimapp prim (map loop args)
       (Esense) -> Esense

       -- Special forms:
       (Esocreturn e) -> Esocreturn (loop e)
       (Esocfinished) -> Esocfinished
       (Ereturn val to via seed aggr) ->
	   -- If there is a seed, recur upon it:
	   let seed' = do s <- seed; return (loop s)
	   in Ereturn (loop val) to via seed' aggr

       -- ERELAY

       (Eemit mbtime tok args) -> Eemit mbtime tok (map loop args)
       (Ecall mbtime tok args) -> Ecall mbtime tok (map loop args)
       (Eactivate tok args)    -> Eactivate    tok (map loop args)

       (Eflood tok)       -> expr	   
       (Eelectleader tok) -> expr

--       _ -> Econst 999
-}


-- Takes: current handler token, token env, local env, and expression
-- Returns: a new replacement expression and a list of new token-handlers.
expand_macros :: Maybe Token -> [Token] -> [Id] -> Expr -> (Expr, [TokHandler])
expand_macros thistok tenv lenv expr = 
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
	       (Eelectleader tok) -> return Esocfinished -- TODO FIXME 

               (Econst c) -> return expr
	       (Evar id)  -> return expr

	       (Elambda ids e) -> error "expand doesn't handle lambda yet"
	       (Elet binds e)  -> 
		   do let lhss = map fst binds
		      rhss <- mapM (loop lenv) (map snd binds)
		      bod <- loop (lhss ++ lenv) e
		      return (Elet (zip lhss rhss) bod)

	       (Eseq es) -> do new_es <- mapM (loop lenv) es
			       return (Eseq new_es)

	       (Eif a b c) -> do a <- loop lenv a 
				 b <- loop lenv b
				 c <- loop lenv c
				 return $ Eif a b c

               (Eassign v e) -> do e <- loop lenv e
				   return $ Eassign v e 


--               (Elambda ...) -> 

--       (Eprimapp Pflood tok) -> Eprimapp prim (map loop args)

               (Eprimapp prim args) -> do args <- mapM (loop lenv) args
					  return $ Eprimapp prim args
	       (Edbg s args) -> do args <- mapM (loop lenv) args 
				   return $ Edbg s args
					  
               (Edist t) -> return (Edist t)
	       (Esense) -> return Esense

               -- Special forms:
	       (Esocreturn e) -> do e <- loop lenv e
				    return $ Esocreturn e 
	       (Esocfinished) -> return Esocfinished
	       (Ereturn val to via seed aggr) ->
		   do val <- loop lenv val
		      case seed of
		        Nothing -> return $ Ereturn val to via Nothing aggr
		        Just s  -> do s <- loop lenv s
			  	      return $ Ereturn val to via (Just s) aggr

               (Eled act col) -> return $ Eled act col

               (Erelay (Just t)) -> return $ Erelay (Just t)
	       (Erelay Nothing) ->
		   return 
		   (case thistok of
		    Nothing -> Erelay Nothing -- FIXME TODO: SHOULD BE ERROR
		    Just t  -> Erelay (Just t))

	       (Eemit mbtime tok args) -> 
		   do args <- mapM (loop lenv) args
		      return $ Eemit mbtime tok args
	       (Ecall mbtime tok args) -> 
		   do args <- mapM (loop lenv) args
		      return $ Ecall mbtime tok args
	       (Eactivate tok args)    -> 
		   do args <- mapM (loop lenv) args
		      return $ Eactivate tok args

-- TODO FINISH:
{-
               (Eif a b c) -> return (Eif ((loop lenv) a) ((loop lenv) b) ((loop lenv) c))
	       (Eprimapp prim args) -> return (Eprimapp prim (map (loop lenv) args))
	       (Esense) -> return Esense
	       (Esocreturn e) -> return (Esocreturn ((loop lenv) e))
	       (Esocfinished) -> return (Esocfinished)
	       (Ereturn val to via seed aggr) ->
	       -- If there is a seed, recur upon it:
		   let seed' = do s <- seed; return ((loop lenv) s)
		   in return (Ereturn ((loop lenv) val) to via seed' aggr)
	       (Erelay mbtok) -> return $ Erelay mbtok
	       (Eemit mbtime tok args) -> Eemit mbtime tok (map (loop lenv) args)
	       (Ecall mbtime tok args) -> Ecall mbtime tok (map (loop lenv) args)
	       (Eactivate tok args)    -> Eactivate    tok (map (loop lenv) args)
	       (Eflood tok)       -> expr	   
	       (Eelectleader tok) -> expr
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
