{- [2004.08.05]

Ok, going to write the assembler in Haskell, anticipating that the
complete system will eventually be in Haskell because I plan to hack
GHC.

This will read

-}

import System.IO.Unsafe
import TM

modname = "TestMachineM"

acceptable_chars = ['_']++['a'..'z']++['0'..'9']

{- NOTE!! This should filter out illegal characters!!-}
tokname (Token name) = filter (\c -> elem c acceptable_chars) name

-- Well, what do we need to do here?  
-- Structure as tasks, assign active messages...
-- flatten and generate code for handlers.

process_consts :: [ConstBind] -> String
process_consts _ = ""

process_handler :: TokHandler -> String
process_handler (t, args, e) = 
  "event TOS_MsgPtr Recv_"++ tokname t ++".receive(TOS_MsgPtr msg) { \n" ++
     bod ++ 
  "}\n\n"
    where bod = 
	    case e of
	     Econst c -> show c 
	     Evar id -> ""
	     Elambda formals e -> ""
	     Eseq e1 e2 -> ""
	     Esocreturn e -> "" 
	     Esocfinished  -> "" 
	     Ereturn e -> "" 
	     Erelay (Just t) -> ""
	     Erelay Nothing -> ""
	     Eemit mt t exps -> ""  
	     Ecall mt t exps -> ""
--    in bod
--	  | Eprimapp (Prim Expr)


process_handlers :: [TokHandler] -> String
process_handlers hnds = foldl (++) "" (map process_handler hnds)

process_startup :: [Token] -> String
process_startup _ = "" 

build_module_header :: [TokHandler] -> String
build_module_header toks = 
    let toknames = map (\ (t,_,_) -> tokname t) toks 
    in "\nmodule " ++ modname ++ " \n {\n" ++ 
       "  provides interface StdControl as Control; \n" ++
       "  uses interface Timer;\n" ++ 
       concat
         (map (\ name -> 
	       "  uses interface SendMsg as Send_"++ name ++"; \n" ++ 
	       "  uses interface ReceiveMsg as Recv_"++ name ++"; \n")
          toknames) ++
       "}\n\n"


build_implementation_header :: [TokHandler] -> String
build_implementation_header toks = 
    let toknames = map (\ (t,_,_) -> tokname t) toks 
    in  "event result_t Send_A.sendDone (TOS_MsgPtr msg, result_t success) {\n" ++
	"  return SUCCESS;\n"

assemble (Pgm consts socconsts socpgm nodetoks startup) = 
    build_header nodetoks ++
    process_consts consts ++ 
    process_consts socconsts ++ 
    process_handlers nodetoks ++ 
    process_startup startup



main = do putStr "Running token machine assembler in Haskell...\n"
	  str <- readFile "test.tm"
	  let expr = (read str :: TMPgm)		     
	  putStr "Tokmac read!\n"
	  putStr str


