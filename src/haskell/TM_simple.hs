module TM_simple where

import TM (Const, Time, Id(Id), Token(Token), Prim, LedColor, LedAction)
--import qualified TM 

type ConstBind = (Id, Basic)
type TokHandler = (Token, [Id], Block)

--data Statement = PrimStat
--  deriving (Eq, Show, Read)

a = (Pgm {
  consts = [((TM.Id "result_1"), (Bconst 3))],
  socconsts=[],
  socpgm= Block [] [],
  nodetoks=
    [((Token "spread-global"), [], 
      Block [] [(Semit Nothing (Token "global-tree") []),
		(Scall Nothing (Just 1000) (Token "spread-global") [])]), 
     ((Token "global-tree"), [], 
      Block [] [Srelay Nothing])],  
  startup=[]
    })


data Pgm = Pgm { consts    :: [ConstBind],
		 socconsts :: [ConstBind],
		 socpgm    :: Block,
		 nodetoks  :: [TokHandler],
		 startup   :: [Token]
	       }
  deriving (Eq, Show, Read)

{-data Prim = Pplus | Pminus | Pmult | Pdiv
	  | Pless | Pgreater | Pleq | Pgeq
	  | Plocdiff | Ploc
--	  | Pflood | Pelectleader
	  | Pdrawmark | Plightup | Prgb
  deriving (Eq, Show, Read) -}
	   
data Block = Block { binds :: [Id],
		     stmts :: [Stmt] 
		   }
  deriving (Eq, Show, Read)


-- The (Maybe Id) in primapp and call indicates a possible associated
-- assignment for the return value of those expressions.
data Stmt = Svoid
	  | Sassign Id Basic
          | Sif Basic [Stmt] [Stmt]
	  | Sled LedAction LedColor
	  | Sdbg String [Basic]
	  | Sdist Token
          | Ssense (Maybe Id)
	  | Sprimapp (Maybe Id) Prim [Basic]               -- returns value!
	  | Scall    (Maybe Id) (Maybe Time) Token [Basic] -- returns value!
	  | Semit (Maybe Time) Token [Basic] -- no value
	  | Sactivate Token [Basic]          -- no value
	  | Srelay (Maybe Token)             -- no value
	  | Sgradreturn {val :: Basic,           -- no value
			 to  :: Token,
			 via :: Token,
			 seed :: Maybe Basic,
			 aggr :: Maybe Token}
--	  | Ssocreturn Basic                 -- no value
--	  | Ssocfinished                     -- no value
	  | Sreturn Basic -- This becomes the return value for a token handler.

  deriving (Eq, Show, Read)

data Basic = -- Stndard forms:
            Bconst Const
	  | Bvar Id
  deriving (Eq, Show, Read)

