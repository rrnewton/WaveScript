
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


import Char
--import Control.Exception
import Control.Monad.State
import Data.List
import Debug.Trace
import GHC.IOBase
import IO
import System
import System.IO.Unsafe
import System.Posix.Files



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

-- Returns the text corresponding to an expression evaluating same
-- value as the given Basic expression.
process_basic :: Basic -> String
process_basic (Bvar (Id str)) = str 
process_basic (Bconst c)      = show c

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
    Sprimapp mbid prim args  -> 
 	let f text = case mbid of 
		     Just (Id id) -> [indent ++ id ++ " = " ++ text ++ ";\n"]
		     Nothing      -> [indent ++ text ++ ";\n"]
	    e1 = process_basic $ args!!0
	    e2 = process_basic $ args!!1
	    e3 = process_basic $ args!!2 -- Lazy evaluation to the rescue!
	    err = [indent ++ "// "++ show prim ++" not available!\n"]
	in case prim of
	   Pplus    -> f $ e1++" + "++e2
	   Pminus   -> f $ e1++" - "++e2
	   Pmult    -> f $ e1++" * "++e2
	   Pdiv     -> f $ e1++" / "++e2
	   Pless    -> f $ e1++" < "++e2
	   Pgreater -> f $ e1++" > "++e2
	   Pleq     -> f $ e1++" <= "++e2
	   Pgeq     -> f $ e1++" >= "++e2
	   Plightup -> err
	   Ploc     -> err
	   Plocdiff -> err
	   _ -> err
--	   _ -> error ("assembler: process_stmt: primitive not handled currently: " ++ show prim)

    Sif b s1 s2 ->
	[ indent ++ "if ( " ++ process_basic b ++ " ) {\n" ] ++
	(concat $ map (process_stmt (indent++"  ") tokargs) s1) ++
	[ indent ++ "} else {\n" ] ++
	(concat $ map (process_stmt (indent++"  ") tokargs) s2) ++
	[ indent ++ "}\n" ]

--    Slambda formals e -> ([build_fun formals e],[],"VOIDNOR")

    
    Ssense (Just (Id id)) -> [ indent ++ id ++ " = call ADC.getData();\n" ]
    Ssense Nothing        -> error "shouldn't have Ssense with no storage location"

    Sgradreturn e to via seed aggr -> 
	let seed' = case seed of 
		    Nothing -> "0"
		    Just s  -> process_basic s 
	    aggr' = case aggr of 
		    Nothing -> "0"
		    Just a  -> tok_id a
        in
	[ 
	 indent ++ "the_packet.type = "++ tok_id to ++";\n", 
	 indent ++ "the_packet_args[0] = "++ process_basic e ++";\n",
         indent ++ "call TMComm_"++ tokname via ++".return_home( "++ 
		 tok_id to ++", "++
	         "sizeof(uint16_t), "++
	         "&the_packet, "++
		 seed' ++", "++
		 aggr' ++");\n" ]

    Srelay (Just t) -> 
        [indent ++ "call TMComm_"++ tokname t ++".relay();\n"]
    Srelay Nothing -> 
	[indent ++"// FAILED relay nothing\n"]

    -- FIXME
    Scall dest time t args -> 
	[ 
--	  indent ++"the_packet.type = " ++ tok_id t ++ ";\n",
	  (case time of
	   Just t -> indent ++"/* Cannot handle timed call right now: */\n"
	   Nothing -> "") ++
	  -- FIXME FILL IN ARG DATA HERE...
          --indent ++"/* Should fill in arg data here... */\n",
	  -- Not going through the FIFO right now...
	  --indent++"call TMComm_"++ tokname t ++".add_msg(the_packet);\n" 
	  --indent ++"/* It's sketchy that I use the_packet for this without copying it... */\n",
	  (case dest of Nothing -> indent; Just (Id s) -> indent ++ s ++ " = ") ++
--	  "call token_"++ tokname t ++"(&the_packet);\n"
	  "call token_"++ tokname t ++"(" ++
			   (concat $ intersperse ", " $ 
			    map process_basic args)
			   ++ ");\n" 
	]

    Sactivate t args -> 
	(indent++"// Cannot time activate..\n")
	: (process_stmt indent tokargs (Scall Nothing Nothing t args))


    Semit (Just time) t exps -> 
	error "assembler: process_expr.  Can't handle optional time argument to emit."

    Semit Nothing t exps -> 
	-- UNLIKE Sendmsg.send my emit function is going to have a message copying semantics.
        -- The buffer must get copied somewhere, so it might as well be there.
	[indent ++"the_packet.type = " ++ tok_id t ++ ";\n",
	 indent ++"/* Should fill in arg data here... */\n",
	 indent ++"call TMComm_"++ tokname t ++
	 ".emit(TOS_BCAST_ADDR, sizeof(uint16_t) /* TODO fix this length */, &the_packet);\n"]

{-
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

--    Ssocreturn b -> []--[ indent ++ "call TMComm.socreturn("++ process_basic b ++");\n" ]
--    Ssocfinished -> []--[ indent ++ "call TMComm.socfinished();\n"]

    Sreturn b -> [ indent ++"return "++ process_basic b ++";\n" ]
    _ -> error ("process_stmt can't handle: " ++ show e)


process_block :: String -> [String] -> Block -> ([FunctionCode],[StatementCode])
process_block indent tokargs (Block locals stmts) = 
    let body = concat $ map (process_stmt indent tokargs) stmts
        defs = concat $ map (process_localdef indent) locals
    in ([], 
	defs ++ body)

process_localdef :: String -> Id -> [String]
process_localdef indent (Id id) = [indent ++"uint16_t "++ id ++";\n"]

build_fun formals body = 
    " foo " 

-------------------------------------------------------------------------------
{- HERES THE MODULE IMPLEMENTATION GENERATOR. -}

-- Well, what do we need to do here?  
-- Structure as tasks, assign active messages...
-- flatten and generate code for handlers.

process_const :: TMS.ConstBind -> String
process_const (Id str, Bconst exp) = "  uint16_t "++str++" = "++show exp++";\n"
process_const _ = error "assembler.hs: process_const: can't handle non Econst expressions atm!!"

process_consts :: [TMS.ConstBind] -> String
process_consts cbs = (foldl (++) "" (map process_const cbs)) ++ "\n"

-- Returns a string for the handler (goes in the switch statement)
-- And another string 
process_handler :: TMS.TokHandler -> ([FunctionCode],StatementCode)
process_handler (t, args, blk) = 
    let (funs,stmts) = process_block "        " (map (\ (Id x)->x) args) blk 
	funname = tokname t
    in (
	--("  command uint16_t token_"++ tokname t  ++"(TOS_MsgPtr msg) { \n" ++ 
	   ("  command uint16_t token_"++ tokname t  ++"("++ 
	    (concat $ intersperse ", " $ map (\ (Id id) -> "uint16_t "++id) args)
	    ++") { \n" ++ 
        (concat  
	 (map2 (\ (Id argname) n -> "        // Argument "++show n++" is '"++show argname++"'\n")
	  args [0..])) ++ 
	(concat stmts)++ 
	 "    return 0; // This is a kind of lame default return value.\n"++
	 "  }\n")
	: funs,
	"      case "++tok_id t++": \n"++
	--- DEBUG CODE
        "        dbg(DBG_USR1, \"TM TestMachine: tok fired: addr %d, type %d, group %d \\n\""++
        ", msg->addr, msg->type, msg->group);\n"++ 
	--- DEBUG CODE
	"        call token_"++ funname ++"("++ 
	(concat $ intersperse ", " $ 
	 map (\ n -> "args["++ show n ++"]") 
	 [1..length args])
	 ++");\n        break;\n")

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
    "  // Like receiveMsg, t must return a TOS_MsgPtr to replace the one it consumes.\n"++
    "  command TOS_MsgPtr TMModule.process_token(TOS_MsgPtr msg) { \n" ++
    "    TM_Payload* payload = (TM_Payload*)msg->data;\n"++
    "    uint16_t* args = (uint16_t*)(payload->args);\n"++
    "    switch (msg->type) {\n"++			      
    (foldl (++) "" bods) ++
    "    }\n"++
    "    return msg;\n" ++ 
    "  }\n\n"


process_startup :: [Token] -> String
process_startup _ = "" 

build_module_header :: [TMS.TokHandler] -> String
build_module_header toks = 
    let toknames = map (\ (t,_,_) -> tokname t) toks 
	arglsts = map (\ (_,ls,_) -> map (\ (Id id) -> id) ls) toks
    in "\nmodule " ++ modname ++ "M \n {\n" ++ 
       "  provides interface StdControl as Control; \n" ++
       "  provides interface TMModule; \n" ++
       "  provides command void start_socpgm();\n"++
--       "  uses interface Timer;\n" ++ 
       concat
       (map2 (\ name argls ->
	       "  provides command uint16_t token_"++ name ++"("++ 
	       (concat $ intersperse ", " $
		map ("uint16_t "++) argls) ++
	       ");\n")
	  toknames arglsts) ++
       "\n"++
       concat
         (map (\ name -> 
	       "  uses interface TMComm as TMComm_"++ name ++"; \n"
--	       "  uses interface SendMsg as Send_"++ name ++"; \n" ++ 
--	       "  uses interface ReceiveMsg as Recv_"++ name ++"; \n"
	      )
          toknames) ++
       "}\n\n"


build_implementation_header :: [TMS.TokHandler] -> [Token] -> String
build_implementation_header toks startup = 
    "\n"

build_implementation_footer :: [TMS.TokHandler] -> [Token] -> String
build_implementation_footer toks startup = 
    "  command result_t Control.init() {\n" ++
    "    return SUCCESS;\n" ++
    "  }\n\n" ++
    "  command result_t Control.start() {\n" ++
--    "    return call Timer.start(TIMER_REPEAT, 1000);\n" ++
    "    // This is where we need to start the socpgm if we're SOC.\n"++
    "    if ( TOS_LOCAL_ADDRESS == BASE_STATION ) {\n"++
--    "      post socpgm();\n"++
    "      call start_socpgm();\n"++
    "    }\n"++
    (foldr 
     (\ t acc -> "    // Clear the arguments here...\n"++
      "    // call TMComm_"++ tokname t ++".add_msg(the_packet);\n" 
      ++ acc)
     "" startup)++
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

build_socfun :: [TMS.ConstBind] -> Block -> String
build_socfun consts (Block locals stmts) =
    let indent = "    " in
    "  task void socpgm() {\n"++ 
    "    dbg(DBG_USR1, \"TM TestMachine: starting soc program...\");\n"++
    process_consts consts ++ 
    -- FIXME TODO INCLUDE FUNS HERE!!!
    -- Generate code for all the statements in 
    (concat $ concat $ map (process_localdef indent) locals) ++
    (concat $ concat $ 
     map (\x-> process_stmt indent [] x)
     stmts) ++ 
    "  }\n\n" ++
    "  command void start_socpgm() {\n"++ 				       
    "    post socpgm();\n"++
    "  }\n\n"

build_module (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
    -- First spit out just a little header:
    "// Automatically generated module for "++show (length nodetoks)++" token handlers:\n"++
    (concat $ map (\ (Token s,_,_) -> "//   "++s++"\n") nodetoks)++
    "\nincludes TestMachine;\n"++
    "includes TokenMachineRuntime;\n\n"++
    build_module_header nodetoks ++
    "implementation {\n" ++ 
    "  TOS_Msg the_packet;\n"++    
    "  uint16_t* the_packet_args = (uint16_t*)(((TM_Payload*)(the_packet.data))->args);\n"++ 
    "\n  // The constant bindings:\n"++
    process_consts consts ++ 
    build_implementation_header nodetoks startup ++
    process_consts socconsts ++ 
    "\n  // Token handlers and their helper functions:\n"++
    process_handlers nodetoks ++ 
    build_socfun socconsts socpgm ++
    -- Put the StdControl stuff in the footer:
    build_implementation_footer nodetoks startup ++
    "}\n"

-------------------------------------------------------------------------------
{- HERES THE CONFIGURATION GENERATOR. -}

build_configuration (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
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

build_header_file (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
    "\n"++
    "#define BASE_STATION 0"++
    "\n"++
    "enum {\n"++
    (concat $ 
     map2 (\ t n -> "  "++tok_id t++" = "++show n++",\n")
     (socret_target : socfinished_target : --global_tree : 
      (map (\ (t,_,_) -> t) nodetoks))
     [1..]) ++
    "};\n"


-------------------------------------------------------------------------------
{- HERES THE MAIN PROGRAM. -}

{- This returns the contents of the NesC module file, config file, and header file. -}
assemble :: TMS.Pgm  -> (String,String,String)
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
			          return (b :: TMPgm))
		         else ioError e)

	  putStr "Expanding token machine... \n" 
	  let prog2 = expand_tm prog
	  putStr "Flattening token machine... \n" 
	  -- This uses the state monad to keep track of 
	  --let prog3 = flatten_tm prog2
	  let (prog3,idcounter) = 
		  runState (flatten_tm prog2) (TM.pgm_largest_id_number prog2)

	  putStr "Running token machine assembler in Haskell... \n" 
	  let (mod,conf,header) = assemble prog3

          let (modfile,conffile,headerfile) = 
		  (modname++"M.nc", modname++".nc", modname++".h")
	  putStr ("\nNow we dump to files: ("++modfile++", "++conffile++", "++headerfile++")...\n\n")

	  writeFile modfile mod
	  writeFile conffile conf
	  writeFile headerfile header
	  putStr "\n.. dumped.\n\n"
