
module TokenMachine where

type Const = Int

type Id = String
type ConstBind = (Id, Const)
type Token = Id 
type Formal = Id
type TokHandler = (Id, [Formal], Expr)


--data Statement = PrimStat
--  deriving (Eq, Show, Read)

data Expr = 
    EReturn Id 
  deriving (Eq, Show, Read)

data TMPgm = Pgm { consts    :: [ConstBind],
		   socconsts :: [ConstBind],
		   socpgm    :: [Expr],
		   nodetoks  :: [TokHandler],
		   startup   :: [Token]
		 }
  deriving (Eq, Show, Read)


x = Pgm { consts    = [],
	  socconsts = [],
	  socpgm    = [],
	  nodetoks  = [],
	  startup   = []
	}

y = Pgm [] [] [] [] []


z = Pgm { consts    = [],
	  socconsts = [],
	  socpgm    = [],
	  nodetoks  = [("tok1", ["x"], Return "x")],
	  startup   = []
	}


