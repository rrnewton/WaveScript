
{- [2004.08.05]

Ok, going to write the assembler in Haskell, anticipating that the
complete system will eventually be in Haskell because I plan to hack
GHC.

This will read

-}

import TM -- Token Machine language definition
import TM_simple as TMS
import Expand
import Flatten
import Utils


import System
import System.IO.Unsafe
import System.Posix.Files
import Char
--import Control.Exception
import GHC.IOBase
import IO
import Data.List

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

type FunctionCode = String
type ExpressionCode = String
type StatementCode = String

process_basic :: Basic -> String
process_basic (Bvar (Id str)) = str

-- This returns both:
--   *) bunch of strings (functions) to be seperately
--     stuck in the top-level of the module implementation, 
--   *) a bunch of strings (statements) which must be placed in the 
--      current basic block above the current expression (local bindings).
--   *) a string (expression) that represents the appropriate value

-- The 2nd argument is the "arg environment" for bound variables.
process_stmt :: String -> [String] -> Stmt -> [StatementCode]
process_stmt indent tokargs e = 
    case e of
    Svoid -> []
    Sassign (Id s) basic -> 
	[indent ++ s ++ " = " ++ process_basic basic ++ ";\n"]
    Sprimapp mbid prim [a, b]  -> 
 	let e1 = process_basic a
	    e2 = process_basic b
	    text = (case prim of
		    Pplus  -> e1++" + "++e2
		    Pminus -> e1++" - "++e2
		    Pmult  -> e1++" * "++e2
		    Pdiv   -> e1++" / "++e2
 		   )
	in case mbid of 
	   Just (Id id) -> [indent ++ id ++ " = " ++ text ++ ";\n"]
	   Nothing      -> [indent ++ text ++ ";\n"]

    Sif b s1 s2 ->
	[ indent ++ "if ( " ++ process_basic b ++ " ) {\n" ] ++
	(concat $ map (process_stmt (indent++"  ") tokargs) s1) ++
	[ indent ++ "} else {\n" ] ++
	(concat $ map (process_stmt (indent++"  ") tokargs) s2) ++
	[ indent ++ "}\n" ]

--    Slambda formals e -> ([build_fun formals e],[],"VOIDNOR")

    
    Ssense (Just (Id id)) -> [ indent ++ id ++ " = (call ADC.getData());\n" ]
    Ssense Nothing        -> error "shouldn't have Ssense with no storage location"

    Sgradreturn e to via seed aggr -> 
	[ indent ++ "(call TMComm_"++ tokname via ++".returnhome("++ 
	  tok_id to ++", "++ 
	  process_basic e ++", "++ 
	  process_basic seed ++", "++
	  tok_id aggr ++");\n"]

    Srelay (Just t) -> ["/* FAILED relay just*/"]
    Srelay Nothing -> ["/* FAILED relay nothing*/"]

    -- FIXME
    Scall Nothing Nothing t args -> 
	[ "the_packet.type = " ++ tok_id t ++ ";\n",
	  -- FILL IN ARG DATA HERE...
	  "call TMComm_"++ tokname t ++".add_msg(the_packet);\n" ]
--"(call " ++ tokname t ++ "(" ++ (concat $ intersperse ", " argexps) ++ "))"

{-
    Eemit (Just time) t exps -> 
	error "assembler: process_expr.  Can't handle optional time argument to emit."
    Eemit Nothing t exps -> 
	([],[],
	 "(call TMComm_"++ tokname t ++
	 ".emit(TOS_BCAST_ADDR, sizeof(uint16_t), &the_packet))")


    Ecall (Just time) t exps -> 
	let (f,s,e) = process_expr indent args (Ecall Nothing t exps)
	in (f,s,
	    --"/* Timed call unhandled !! */  " ++ 
	    e)

    -- Same as call for the moment!
    Eactivate t exps -> 
	let (funs,stmts,argexps) = 
		foldl (\ (f,s,es) expr -> 
		       let (f2,s2,e) = process_expr indent args expr
		       in (f++f2, s++s2, e:es))
		([],[],[]) exps
	in (funs, 
	    stmts ++ [ indent++"the_packet.type = " ++ tok_id t ++ ";\n"],
	    "(call TMComm.add_msg(the_packet))")

    _ -> error ("process_expr: unhandled expression!: "++show e)
-}

    Ssocreturn b -> [ indent ++ "call TMComm.socreturn("++ process_basic b ++");\n" ]
    Ssocfinished -> [ indent ++ "call TMComm.socfinished();\n"]

    Sreturn b -> [ indent ++"return "++ process_basic b ++";\n" ]


process_block :: String -> [String] -> Block -> ([FunctionCode],[StatementCode])
process_block indent tokargs (Block locals stmts) = 
    let body = concat $ map (process_stmt indent tokargs) stmts
        defs = map (\ (Id id) -> indent ++"uint8_t "++ id ++";\n") locals
    in ([], defs ++ body)

build_fun formals body = 
    " foo " 

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
process_handler :: TMS.TokHandler -> ([FunctionCode],StatementCode)
process_handler (t, args, blk) = 
    let (funs,stmts) = process_block "        " (map (\ (Id x)->x) args) blk 
    in (funs,
	"      case "++tok_id t++": \n"++
        (concat  
	 (map2 (\ (Id argname) n -> "        // Argument "++show n++" is '"++show argname++"'\n")
	 args [0..]))++
	(concat stmts)++
	"        break;\n")

--  "    int i;\n"++
--  "    uint8_t length = msg->length;\n"++
--  "    uint8_t type = msg->type;\n"++
--  "    TM_Payload payload = *((TM_Payload *)msg->data);\n"++
--     (snd $ process_expr e) ++ 

--    in bod
--	  | Eprimapp (Prim Expr)

process_handlers :: [TMS.TokHandler] -> String
process_handlers hnds = 
    let funs = concat $ map (fst . process_handler) hnds
	bods = map (snd . process_handler) hnds
    in
    (concat (map (++"\n") funs))++
    "\n  // This is the main token-processing function:\n"++
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
    -- TODO INCLUDE FUNS HERE!!!
    (concat $ concat $ 
     map (\x-> let (_,stmts) = process_stmt "    " [] x 
	  in stmts) exprs) ++ 
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
    "  TOS_Msg the_packet;\n"++    
    "\n  // The constant bindings:\n"++
    process_consts consts ++ 
    build_implementation_header nodetoks ++
    process_consts socconsts ++ 
    "\n  // Token handlers and their helper functions:\n"++
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





main = do return 3