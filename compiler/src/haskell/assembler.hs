{- [2004.08.05]

Ok, going to write the assembler in Haskell, anticipating that the
complete system will eventually be in Haskell because I plan to hack
GHC.

This will read

-}

import TM

main = do putStr "Running token machine assembler in Haskell...\n"
	  str <- readFile "test.tm"
	  let expr = (read str :: TMPgm)
	  putStr "Tokmac read!\n"
	  putStr str



