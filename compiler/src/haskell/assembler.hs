
{- [2004.08.05]

Ok, going to write the assembler in Haskell, anticipating that the
complete system will eventually be in Haskell because I plan to hack
GHC.

This will read

-}

import System
import System.IO.Unsafe
import System.Posix.Files
import Char
--import Control.Exception
import GHC.IOBase
import IO
--import Data.List

import TM -- Token Machine language definition

modname = "TestMachine"

acceptable_chars = ['_']++['a'..'z']++['0'..'9']

{- NOTE!! This should filter out illegal characters!!-}
tokname :: Token -> String
tokname (Token name) = filter (\c -> elem c acceptable_chars) name

-- This takes a token to a string identifying its ActiveMessage number:
tok_id :: Token -> String
tok_id t = "AM_" ++ (map toUpper (tokname t))

map2 f [] _ = []
map2 f _ [] = []
map2 f (a:ta) (b:tb) = (f a b) : (map2 f ta tb) 

-------------------------------------------------------------------------------
{- HERES THE EXPRESSION GENERATOR. -}

-- This returns both a bunch of strings (functions) to be seperately
-- stuck in the top-level of the module implementation, and a string
-- that goes within the receive-message handler or wherever.
-- The 2nd argument is the "arg environment" for bound variables.
process_expr :: Expr -> [String] -> ([String],String)
process_expr e args = 
    case e of
    -- Constants restricted to 16 bit numbers for now:
    Econst c -> ([],"    return "++show c++"\n")
    Evar id -> ([],"    return "++show id++"\n")
    Elambda formals e -> ([],build_fun formals e)
    Eseq e1 e2 -> 
	let (a,b) = process_expr e1 args
	    (x,y) = process_expr e2 args
	in (a++x, b++y)	    

    Esocreturn e -> ([],"") 
    Esocfinished  -> ([],"") 
    Ereturn e -> ([],"") 
    Erelay (Just t) -> ([],"") 
    Erelay Nothing -> ([],"") 

    Eemit mt t exps -> ([],"    TMComm.emit("++tok_id t++");\n")
    Ecall mt t exps -> ([],"")


build_fun formals body = 
    " foo " 

--(Eseq (Eemit Nothing (Token "global-tree") []) 
--       (Ecall (Just 1000) (Token "spread-global") []))

-------------------------------------------------------------------------------
{- HERES THE MODULE IMPLEMENTATION GENERATOR. -}

-- Well, what do we need to do here?  
-- Structure as tasks, assign active messages...
-- flatten and generate code for handlers.

process_const :: ConstBind -> String
process_const (Id str, Econst exp) = "  uint8_t "++str++" = "++show exp++";\n"
process_const _ = error "assembler.hs: process_const: can't handle non Econst expressions atm!!"

process_consts :: [ConstBind] -> String
process_consts cbs = (foldl (++) "" (map process_const cbs)) ++ "\n"

-- Returns a string for the handler (goes in the switch statement)
-- And another string 
process_handler :: TokHandler -> ([String],String)
process_handler (t, args, e) = 
    let (funs,bod) = process_expr e (map (\ (Id x)->x) args) 
    in (funs,
	"    case "++tok_id t++": \n"++
        (concat  
	 (map2 (\ (Id argname) n -> "      // Argument "++show n++" is '"++show argname++"'\n")
	 args [0..]))++
	bod++
	"    break;\n")

--  "    int i;\n"++
{-  "    uint8_t length = msg->length;\n"++
  "    uint8_t type = msg->type;\n"++
  "    TM_Payload payload = *((TM_Payload *)msg->data);\n"++
     (snd $ process_expr e) ++ -}

--    in bod
--	  | Eprimapp (Prim Expr)


process_handlers :: [TokHandler] -> String
process_handlers hnds = 
    let funs = concat $ map (fst . process_handler) hnds
	bods = map (snd . process_handler) hnds
    in
    (concat (map (++"\n") funs))++
    "\n\n"++
    "  command TOS_MsgPtr TMModule.process_token(TOS_MsgPtr msg) { \n" ++
    "    TM_Payload* payload = (TM_Payload*)msg->data;\n"++
    "    int16_t* args = (int16_t*)(payload->args);\n"++
    "    switch (msg->type) {\n"++			      
    (foldl (++) "" bods) ++
    "    }\n"++
    "    return msg;\n" ++ 
    "  }\n\n"

process_startup :: [Token] -> String
process_startup _ = "" 

build_module_header :: [TokHandler] -> String
build_module_header toks = 
    let toknames = map (\ (t,_,_) -> tokname t) toks 
    in "\nmodule " ++ modname ++ "M \n {\n" ++ 
       "  provides interface StdControl as Control; \n" ++
       "  provides interface TMModule; \n" ++
       "  provides command void start_socpgm();\n"++
--       "  uses interface Timer;\n" ++ 
       concat
         (map (\ name -> 
	       "  uses interface TMComm as TMComm_"++ name ++"; \n"
--	       "  uses interface SendMsg as Send_"++ name ++"; \n" ++ 
--	       "  uses interface ReceiveMsg as Recv_"++ name ++"; \n"
	      )
          toknames) ++
       "}\n\n"


build_implementation_header :: [TokHandler] -> String
build_implementation_header toks = 
    "  command result_t Control.init() {\n" ++
    "    return SUCCESS;\n" ++
    "  }\n\n" ++
    "  command result_t Control.start() {\n" ++
--    "    return call Timer.start(TIMER_REPEAT, 1000);\n" ++
    "    return SUCCESS;\n"++
    "  }\n\n" ++
    "  command result_t Control.stop() {\n" ++
--    "    return call Timer.stop();\n" ++
    "    return SUCCESS;\n"++
    "  }\n\n" 
-- ++   (concat $
--     map (\ name -> 
--	  "  event result_t TMComm_"++name++".sendDone (TOS_MsgPtr msg, result_t success) {\n" ++
--	  "    return SUCCESS;\n" ++
--	  "  }\n\n")
--     (map (\ (t,_,_) -> tokname t) toks))


build_socfun consts exprs = 
    "  task void socpgm() {\n"++ 
    process_consts consts ++ 
    (concat (map (\x-> snd (process_expr x [])) exprs)) ++ 
    "  }\n\n" ++
    "  command void start_socpgm() {\n"++ 				       
    "    post socpgm();\n"++
    "  }\n\n"

build_module (Pgm consts socconsts socpgm nodetoks startup) = 
    -- First spit out just a little header:
    "// Automatically generated module for "++show (length nodetoks)++" token handlers:\n"++
    (concat $ map (\ (Token s,_,_) -> "//   "++s++"\n") nodetoks)++
    "\nincludes TestMachine;\n"++
    "includes TokenMachineRuntime;\n\n"++
    build_module_header nodetoks ++
    "implementation {\n" ++ 
    process_consts consts ++ 
    build_implementation_header nodetoks ++
    process_consts socconsts ++ 
    process_handlers nodetoks ++ 
    build_socfun socconsts socpgm ++
    "}\n"

-------------------------------------------------------------------------------
{- HERES THE CONFIGURATION GENERATOR. -}

build_configuration (Pgm consts socconsts socpgm nodetoks startup) = 
    "includes TestMachine;\n"++
    "includes TokenMachineRuntime;\n\n"++
    "configuration "++modname++"\n"++
    "{\n"++
    "}\n"++
    "implementation\n"++
    "{\n"++
    "  components "++modname++"M, Main, TimerC, BasicTMComm, GenericComm as Comm;\n"++
    "\n"++
    "  Main.StdControl -> TestMachineM.Control;\n"++
    "  Main.StdControl -> Comm;\n"++
    "  Main.StdControl -> TimerC;\n"++
    "\n"++
    "  BasicTMComm.TMModule -> TestMachineM.TMModule;\n"++
    "\n"++
    (concat $
     map2 (\ name number -> 
	   "  TestMachineM.TMComm_"++name++" -> BasicTMComm.TMComm["++show number++"];\n"
--	   "  TestMachineM.Send_"++name++" -> Comm.SendMsg["++show number++"];\n"++
--	   "  TestMachineM.Recv_"++name++" -> Comm.ReceiveMsg["++show number++"];\n\n"
	  )
     (map (\ (t,_,_) -> tokname t) nodetoks)   [1..]) ++
    --  TestMachineM.Timer -> TimerC.Timer[unique("Timer")];
    "}\n"

-------------------------------------------------------------------------------

build_header_file (Pgm consts socconsts socpgm nodetoks startup) = 
    "enum {\n"++
    (concat $ 
     map2 (\ (t,_,_) n -> "  "++tok_id t++" = "++show n++",\n")
     nodetoks [1..])++
    "};\n"


-------------------------------------------------------------------------------
{- HERES THE MAIN PROGRAM. -}

{- This returns the contents of the NesC module file, config file, and header file. -}
assemble :: TMPgm  -> (String,String,String)
assemble prog = (build_module prog, build_configuration prog, build_header_file prog)


--    process_startup startup
		    

main = do args <- System.getArgs 
	  let argstr = concatMap (++" ") args
	  putStr ("System args ("++ show (length args)++") are: " ++ argstr ++ "\n")
	  let filename = (case args of 
			  []     -> "test.tm"
			  [fn] -> fn
			  _   -> error ("Too many arguments for assembler!: "++argstr))
--	  exists <- fileExist filename
{-	  prog <- if exists 
		  then (do str <- readFile filename
			   putStr ("Read tokmac from file: "++filename++"\n")
			   return (read str :: TMPgm))
		  else (return (a :: TMPgm))-}
	  prog <- catch (do str <- readFile filename
			    putStr ("Read tokmac from file: "++filename++"\n")
			    return (read str :: TMPgm))
		  (\e -> if IO.isDoesNotExistError e 
     		         then (do putStr "File not found.  Compiling default tokmac.\n"
			          return (a :: TMPgm))
		         else ioError e)

	  putStr "Running token machine assembler in Haskell... \n" 
	  let (mod,conf,header) = assemble prog

          let (modfile,conffile,headerfile) = 
		  (modname++"M.nc", modname++".nc", modname++".h")
	  putStr ("\nNow we dump to files: ("++modfile++", "++conffile++", "++headerfile++")...\n\n")

	  writeFile modfile mod
	  writeFile conffile conf
	  writeFile headerfile header
	  putStr "\n.. dumped.\n\n"

