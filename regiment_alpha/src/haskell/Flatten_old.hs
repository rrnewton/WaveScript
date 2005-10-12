{-  [2004.10.28]
This simply flattens out the token machine into single assignment form.
-}

module Flatten where

import Utils
import TM
import TM_simple as TMS 

flatten_tm :: TMPgm -> TMS.Pgm
flatten_tm (TM.Pgm consts socconsts socpgm nodetoks startup) =     
    let toknames = map (\ ((Token x), _,_) -> x) nodetoks 
	names = new_name_stream toknames "flat_" in
    let (socpgm2,names2) = foldl  (\ (Block binds stmts, names) e -> 
				   let (Block binds2 stmts2, names') = pe names e 
				   in  (Block (binds++binds2) (stmts++stmts2), names'))
			   (Block [] [], names) socpgm in
    let (nodetoks2,names3) = foldl (\ (acc,names) th ->
				    let (th',names') = pth names th 
				    in (th':acc,names'))
			     ([],names2) nodetoks 
    in (TMS.Pgm consts socconsts socpgm2 nodetoks2 startup)


-- Process TokenHandler
pth :: [String] -> TM.TokHandler -> (TMS.TokHandler,[String])
pth names (t,ids,e) = ((t, ids, fst $ pe names e),
		                snd $ pe names e)

-- Process Expression
pe :: [String] -> Expr -> (Block,[String])
pe names e = 
    case e of 
       (Econst c) -> (Svoid, names)
       (Evar id)  -> (Svoid, names)
--       (Elambda ids e) -> error "Flatten.pe: should not have Elambda at this point"

{-       (Elet binds e)  -> 
	   Elet [ (lhs, pe rhs) | lhs <- map fst binds,
		                    rhs <- map snd binds ]
		 (pe (map fst  e)
-}

       (Eseq exprs) -> Eseq (map pe exprs)

       (Eif a b c) -> Eif (pe a) (pe b) (pe c)

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

