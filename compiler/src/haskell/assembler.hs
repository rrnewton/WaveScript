
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
import Data.List

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

type FunctionCode = String
type ExpressionCode = String
type StatementCode = String

-- This returns both:
--   *) bunch of strings (functions) to be seperately
--     stuck in the top-level of the module implementation, 
--   *) a bunch of strings (statements) which must be placed in the 
--      current basic block above the current expression (local bindings).
--   *) a string (expression) that represents the appropriate value

-- The 2nd argument is the "arg environment" for bound variables.
process_expr :: String -> [String] -> Expr -> ([FunctionCode],[StatementCode],ExpressionCode)
process_expr indent args e = 
    case e of
    -- Constants restricted to 16 bit numbers for now:
    Econst c -> ([],[],show c)
    Evar (Id var) -> 
	case elemIndex var args of
	-- This just means it's a *let bound* variable.
	Nothing  -> ([],[],show var)
	Just ind -> ([],[],"args["++show ind++"]")

    Elambda formals e -> ([build_fun formals e],[],"VOIDNOR")

    Elet binds body ->
	let (funs1,stmts1,bodcode)  = process_expr indent args body
	    (funs2,stmts2) = 
		foldl (\ (funacc,stmtacc) (lhs,rhs) -> 
		       let (rhsfun,rhsstmt,code) = process_expr indent args rhs
		       in (funacc ++ rhsfun,
			   stmtacc ++ rhsstmt ++ [indent ++ "int16_t lhs = "++code++"\n"]))
		([],[]) binds
	in (funs1++funs2,
	    stmts1++stmts2,
	    bodcode)

    Eseq e1 e2 -> 
	let (funs1,stmts1)       = process_stmt indent args e1
	    (funs2,stmts2,ecode) = process_expr indent args e2 
	in (funs1++funs2, stmts1++stmts2, ecode)

    Eprimapp prim [a, b]  -> 
	let (f1,s1,e1) = process_expr indent args a
	    (f2,s2,e2) = process_expr indent args b
        in
	(f1++f2, s1++s2,
	 case prim of
	 Pplus  -> e1++" + "++e2
	 Pminus -> e1++" - "++e2
	 Pmult  -> e1++" * "++e2
	 Pdiv   -> e1++" / "++e2
	 )
    
  
    Esense -> ([],[],"(call ADC.getData())")

--    Esocreturn e -> ([],[],"/* FAILED socreturn */") 
--    Esocfinished  -> ([],[],"/* FAILED socfinished*/") 
--    Ereturn e to via seed aggr -> ([],[],"/* FAILED return */") 
--    Erelay (Just t) -> ([],[],"/* FAILED relay just*/") 
--    Erelay Nothing -> ([],[],"/* FAILED relay nothing*/")

    Esocreturn e -> let (f,s,bod) = process_expr indent args e
		    in ([],[],"(call TMComm.socreturn("++ bod ++"))") 
    Esocfinished  -> ([],[],"(call TMComm.socfinished())")
    Ereturn e to via seed aggr -> 
	let (f,s,bod) = process_expr indent args e
        in ([],[],"(call TMComm.returnhome("++ bod ++"))")
    Erelay (Just t) -> ([],[],"(call TMComm.socrelay())")		       
    Erelay Nothing -> ([],[],"(call TMComm.socrelay())")

    Eemit (Just time) t exps -> 
	error "assembler: process_expr.  Can't handle optional time argument to emit."
    Eemit Nothing t exps -> 
	([],[],
	 "(call TMComm_"++ tokname t ++
	 ".emit(TOS_BCAST_ADDR, sizeof(uint16_t), &the_packet))")

    -- FIXME
    Ecall Nothing t exps -> 
	let (funs,stmts,argexps) = 
		foldl (\ (f,s,es) expr -> 
		       let (f2,s2,e) = process_expr indent args expr
		       in (f++f2, s++s2, e:es))
	        ([],[],[]) exps
	in (funs, 
	    stmts,  
	    "the_packet.type = " ++ tok_id t ++ ";\n" ++
            indent ++ "call TMComm.add_msg(the_packet)")
--	    stmts ++ [ indent++"the_packet.type = " ++ tok_id t ++ ";\n"],
--	    "(call TMComm.add_msg(the_packet))")
--	    "(call " ++ tokname t ++ "(" ++ (concat $ intersperse ", " argexps) ++ "))")

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


-- <TODO>: FIXME FIX CONDITIONALS

-- This is just like process_expr but takes a tail-context argument
-- and makes sure that the "return" statement falls where it should.
-- It does not produce any expression-code because it's not used in value context.
-- It will produce a basic block with at least one "return" expression.
process_tail :: String -> [String] -> Expr-> ([FunctionCode],[StatementCode])
process_tail indent args e = 
    let (funs,stmts,bod) = process_expr indent args e 
    in (funs,stmts ++ [indent ++ "return " ++ bod ++ ";\n"])

{-    let (funs,bod) = process_expr indent args e 
	ret = "return "++ bod 
    in	
     (funs,
      case e of
        Econst _    -> ret
        Evar _      -> ret
        Ecall _ _ _ -> ret

        Elambda _ _ -> error "process_tail: can't handle lambda"

	Eseq e1 e2 -> 
	    let (funs1,stmts1)       = process_stmt indent args e1
		(funs2,stmts2,ecode) = process_expr indent args e2 
	    in (funs1++funs2, stmts1++stmts2, code)

	    let (_,b) = process_expr indent args e1 
		(_,y) = process_tail indent args e2 
	    in b++y

        -- These should have no return value:
	Eemit time t exps -> "return void;\n"
	Erelay time       -> "return void;\n"
      )-}


process_stmt :: String -> [String] -> Expr-> ([FunctionCode],[StatementCode])
process_stmt indent args e = 
    let (funs,stmts,ecode) = process_expr indent args e 
    in (funs, stmts ++ [(indent ++ ecode ++ ";\n")])

{-
    Esocreturn e -> ([],"") 
    Esocfinished  -> ([],"") 
    Ereturn e -> ([],"") 
-}


{-foob x = case x of
	 3 -> 99
	 4 -> 100

newb = case 3 of
	 3  4 -> 100
-}

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
process_handler :: TokHandler -> ([FunctionCode],StatementCode)
process_handler (t, args, e) = 
    let (funs,stmts) = process_stmt "        " (map (\ (Id x)->x) args) e 
    in (funs,
	"      case "++tok_id t++": \n"++
        (concat  
	 (map2 (\ (Id argname) n -> "        // Argument "++show n++" is '"++show argname++"'\n")
	 args [0..]))++
	(concat stmts)++
	"        break;\n")

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

