-- Token Machines

-- [2004.08.06]
-- For the moment these are not flattened; they may have complex operands.
-- NesC should be fine with that.
--import Text.PrettyPrint.HughesPJ

module TM where

import Utils

-- For now our only constants are 16 bit integers.

type Const = Int
type ConstBind = (Id, Expr)
--type Formal = Id
type TokHandler = (Token, [Id], Expr)
type Time = Float

data Id = Id String
  deriving (Eq, Show, Read)

data Token = Token String
  deriving (Eq, Show, Read)

--data Statement = PrimStat
--  deriving (Eq, Show, Read)

data TMPgm = Pgm { consts    :: [ConstBind],
		   socconsts :: [ConstBind],
		   socpgm    :: [Expr],
		   nodetoks  :: [TokHandler],
		   startup   :: [Token]
		 }
  deriving (Eq, Show, Read)

data Prim = Pplus | Pminus | Pmult | Pdiv
	  | Pless | Pgreater | Pleq | Pgeq
	  | Plocdiff | Ploc
--	  | Pflood | Pelectleader
	  | Pdrawmark | Plightup | Prgb
	  --Pamap | Pafold
  deriving (Eq, Show, Read)

data Expr = -- Stndard forms:
            Econst Const
	  | Evar Id
	  | Elambda [Id] Expr
	  | Elet [(Id,Expr)] Expr
	  | Eassign Id Expr
	  | Eseq [Expr]

          | Eif Expr Expr Expr

	  | Eprimapp Prim [Expr]
          | Esense

            -- Special forms:
	  | Esocreturn Expr
	  | Esocfinished	  
	  | Ereturn {val :: Expr,
		     to  :: Token,
		     via :: Token,
		     seed :: Maybe Expr,
		     aggr :: Maybe Token}
	  | Erelay (Maybe Token)
	  | Eemit (Maybe Time) Token [Expr]
	  | Ecall (Maybe Time) Token [Expr]
	  | Eactivate Token [Expr]
--	  | Eflood Token [Expr]
	  | Eflood Token 
	  | Eelectleader Token

  deriving (Eq, Show, Read)


--data UNOP = 
data BINOP = TMPplus | TMPmult
--data TRIOP = 



-- Some CONSTANTS:

-- This flagrantly assumes that "socret_target" is an unused name.
socret_target = Token "socret_target"
-- Same flagrancy:
socfinished_target = Token "socfinished_target"

-- This had better match what the frontend spits out:
global_tree = Token "globaltree"


--------------------------------------------------------------------------------

pgm_largest_id_number :: TMPgm -> Int
pgm_largest_id_number p = largest_number $ collect_ids p

collect_ids :: TMPgm -> [String]
collect_ids (Pgm consts socconsts socpgm nodetoks startup) = 
    (map (\ (Id id, _) -> id) consts) ++
    (map (\ (Id id, _) -> id) socconsts) ++
    (concat $ map expr_collect_ids socpgm) ++
    (concat $ map (\ (Token t,args,e) -> 
		   t : map (\ (Id id) -> id) args 
		   ++ expr_collect_ids e)
              nodetoks)

expr_collect_ids :: Expr -> [String]
expr_collect_ids = f 
    where 
    f x = case x of 
       (Econst c) -> [];
       (Evar (Id s))  -> [s]
       (Elambda ids e) -> map (\ (Id s) -> s) ids ++ f e
       (Elet binds e)  -> (concat $ map (\ ((Id s),rhs) -> s : f rhs) binds) ++ f e
       (Eseq exprs) -> concat $ map f exprs
       (Eif a b c) -> f a ++ f b ++ f c
       (Eprimapp prim args) -> concat $ map f args
       (Esense) -> []
       (Esocreturn e) -> f e
       (Esocfinished) -> []
       (Ereturn val _ _ (Just seed) _) -> f val ++ f seed
       (Ereturn val _ _  Nothing    _) -> f val 
       (Erelay mbtok) -> []
       (Eemit mbtime (Token s) args) -> s : (concat $ map f args)
       (Ecall mbtime (Token s) args) -> s : (concat $ map f args)
       (Eactivate    (Token s) args) -> s : (concat $ map f args)
       (Eelectleader (Token s)) -> [s]
       (Eflood (Token s))       -> [s]


--------------------------------------------------------------------------------

x = Pgm { consts    = [],
	  socconsts = [],
	  socpgm    = [],
	  nodetoks  = [],
	  startup   = []
	}

y = Pgm [] [] [] [] []


{-z = Pgm { consts    = [(Id "woot",Econst 3)],
	  socconsts = [(Id "foot",Econst 4)],
	  socpgm    = [],
	  nodetoks  = [(Token "tok1", [Id "x"], Ereturn (Evar (Id "x")))],
	  startup   = []
	}-}

s = "(Pgm {} " ++ 
    ")"

r = Econst
    3

doit = do s <- readFile "test.tm";
	  putStr "Read file:\n";
	  putStr s;
	  let stuff = ((read s) :: TMPgm)
	  putStr (show stuff);
	  putStr "\n That's that...\n"
	  return stuff


--  consts = [(result_2, (Econst 3))],
--  socconsts=[],
--  socpgm=[(Esocreturn (Evar (Id \"result_2\"))), Esocfinished],
--  nodetoks=[((Token \"spread-global\"), [], (Ebegin [(Eemit Nothing (Token \"global-tree\") []), (Ecall (Just 1000) (Token \"spread-global\") [])])), ((Token \"global-tree\"), [], (Erelay Nothing))],  startup=[]
--})"


{-a = (Pgm {
  consts = [((Id "result_1"), (Econst 3))],
  socconsts=[],
  socpgm=[(Esocreturn (Evar (Id "result_1"))), Esocfinished],
  nodetoks=
    [((Token "spread-global"), 
      [], 
      (Eseq [(Eemit Nothing (Token "global-tree") []),
	     (Ecall (Just 1000) (Token "spread-global") [])])), 
     ((Token "global-tree"), [], 
      (Erelay Nothing))],  
  startup=[]
    })
-}

{-v = (Pgm {
      consts = [],
      socconsts=[],
      socpgm=[(Ecall Nothing (Token "spread-global") [])],
      nodetoks=[((Token "f_token_tmpworld_7"), [], (Ecall Nothing (Token "m_token_tmpworld_7") [])),
		((Token "spark-world"), [], (Ecall Nothing (Token "m_token_tmpworld_7") [])),
		((Token "tmpfunc_8"), [(Id "a_1")], (Elet [((Id "result_4"), (Esense))] (Evar (Id "result_4")))),
		((Token "m_token_tmpworld_7"), [], (Eactivate (Token "f_token_tmprmap_9") [])),
		((Token "f_token_tmprmap_9"), [], 
		 (Eseq [(Ecall Nothing (Token "m_token_tmprmap_9") 
			 [(Ecall Nothing (Token "tmpfunc_8") [(Evar (Id "this"))])])
			(Ecall (Just 10.0) (Token "f_token_tmprmap_9") [])])),
		((Token "tmpfunc_10"), [(Id "a_3"), (Id "b_2")], 
		 (Elet [((Id "result_5"), 
			 (Eprimapp Pplus [(Evar (Id "a_3")), (Evar (Id "b_2"))]))]
		  (Evar (Id "result_5")))),
		((Token "m_token_tmprmap_9"), [(Id "v")], 
		 (Ecall Nothing (Token "f_token_result_6") [(Evar (Id "v"))])),
		((Token "f_token_result_6"), [(Id "v")], 
		 (Ereturn (Evar (Id "v")) 
		          (Token "m_token_result_6") 
		          (Token "global-tree") 
		          (Econst 0) 
		          (Token "tmpfunc_10"))),
		((Token "m_token_result_6"), [(Id "v")], (Esocreturn (Evar (Id "v")))),
		((Token "leaf-pulsar_tmpworld_7"), [], 
		 (Eseq [(Ecall Nothing (Token "f_token_tmpworld_7") []), 
			(Ecall (Just 1.0) (Token "leaf-pulsar_tmpworld_7") [])])),
		((Token "spread-global"), [], 
		 (Eseq [(Eemit Nothing (Token "global-tree") []),
			(Ecall (Just 1000) (Token "spread-global") [])])),
		((Token "global-tree"), [], (Erelay Nothing))],  
      startup=[(Token "leaf-pulsar_tmpworld_7"), (Token "spark-world")]
     })
-}


b = (Pgm {
  consts = [],
  socconsts=[],
  socpgm=[(Ecall Nothing (Token "spread-global") [])],
  nodetoks=[((Token "f_token_result_1"), [], 
	     (Eseq [(Eflood (Token "constok_4")), 
		    (Eprimapp Pdrawmark [(Econst 30), (Eprimapp Prgb [(Econst 0), (Econst 100), (Econst 100)])]),
		    (Econst 25966)])),
	    ((Token "constok_4"), [], 
	     (Eif (Eprimapp Pless [(Eprimapp Plocdiff [(Eprimapp Ploc []), (Econst 30)]), 
				   (Econst 10)]) 
	      (Eelectleader (Token "m_token_result_1")) 
	      (Econst 0))),
	    ((Token "m_token_result_1"), [], 
	     (Eseq [(Esocreturn (Econst 49)),
		    (Eprimapp Plightup [(Econst 0), (Econst 255), (Econst 255)]),
		    (Econst 25966)])),
	    ((Token "leaf-pulsar_result_1"), [], 
	     (Eseq [(Ecall Nothing (Token "f_token_result_1") []),
		    (Ecall (Just 1000) (Token "leaf-pulsar_result_1") [])])),
	    ((Token "spread-global"), [], 
	     (Eseq [(Eemit Nothing (Token "global-tree") []),
		    (Ecall (Just 1000) (Token "spread-global") [])])),
	    ((Token "global-tree"), [], (Erelay Nothing))],
 startup=[(Token "leaf-pulsar_result_1")]
})

