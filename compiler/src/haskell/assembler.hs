{- [2004.08.05]

Ok, going to write the assembler in Haskell, anticipating that the
complete system will eventually be in Haskell because I plan to hack
GHC.

This will read

-}

import TM

-- Well, what do we need to do here?  
-- Structure as tasks, assign active messages...
-- flatten and generate code for handlers.
assemble (Pgm consts socconsts socpgm nodetoks startup) = 
    0


main = do putStr "Running token machine assembler in Haskell...\n"
	  str <- readFile "test.tm"
	  let expr = (read str :: TMPgm)
	  putStr "Tokmac read!\n"
	  putStr str



