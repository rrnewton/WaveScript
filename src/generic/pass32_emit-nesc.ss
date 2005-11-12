;; Pass Emit NesC [2005.11.09]

;; Generate NesC code.  Porting this from the haskell code.













(define-syntax ++
  (lambda (x)
    (syntax-case x ()
		 [id (identifier? #'id) #'string-append]
		 [(_ E ...) #'(string-append E ...)])))
(define (prepend-all x ls)
  (if (null? ls) ()
      (cons (++ x (car ls)) (prepend-all x (cdr ls)))))
    

(define modname "TestMachine")
(define acceptable_chars 
  '(#\_ 
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;; String -> String
(define (mangle-name str)
  (list->string (filter (lambda (c) (memq c acceptable_chars)) 
		  (string->list str))))
;; This should filter out illegal characters!!
(define (tokname x)
    (match x
      [(tok ,t ,_) (mangle-name (symbol->string t))]
      [,s (guard (string? s)) (mangle-name s)]
      [,v (symbol? v) (mangle-name (symbol->string v))]))

(define (upcase s) (list->string (map char-upcase (string->list s))))

;; This takes a token to a string identifying its ActiveMessage number:
(define (tok_id t)  (++ "AM_" (upcase (tokname t))))


(define token-numberer
  (lambda ()
    (let ((counter 0)
          (table (make-default-hash-table)))
      (lambda (s)
        (let ((x (hashtab-get table s)))
          (if x x
              (begin 
                (if (> counter 255)
                    (error 'emit-nesc:token->number
                           "too many distinct tokens, 257th token: ~s" s))
                (hashtab-set! table s counter)
                (set! counter (add1 counter))
                (sub1 counter))))))))

(define token->number (token-numberer))

(define socret_target (mangle-name "SOC-return-handler"))
;socfinished_target 

;; -------------------------------------------------------------------------------
;; HERES THE EXPRESSION GENERATOR. 

					;type FunctionCode = String
					;type ExpressionCode = String
					;type StatementCode = String


;; Returns the text corresponding to an expression evaluating same
;; value as the givoen Basic expression.
(define Basic 
  (lambda (x)
    (match x
      [,v (guard (symbol? v)) (symbol->string v)]
      [(quote ,c) (guard (constant? c)) (format "~a" c)]
      ;; This is occuring in operand position, for now we allow only 256
      ;; tokens and 256 subids, this fitting the whole thing in a uint16_t
      [(tok ,t ,[id]) (format "(~a + ~a)" (* 256 (token->number t)) id)]
      )))

(define basic?
  (lambda (x)
    (match x
      [,v (guard (symbol? v)) #t]
      [(quote ,c) (guard (constant? c)) #t]
      [(tok ,t ,id) #t]
      [,else #f])))


;; This returns both:
;;   *) bunch of strings (functions) to be seperately
;;     stuck in the top-level of the module implementation, 
;;   *) a bunch of strings (statements) which must be placed in the 
;;      current basic block above the current expression (local bindings).
;;   *) a string (expression) that represents the appropriate value

;;  The 2nd argument is the "arg environment" for bound variables.
;process_stmt :: String -> [String] -> TMS.Stmt -> [StatementCode]
(define (Statement indent tokargs)
  (lambda (e)
    (match e
      [,b (guard (basic? b)) (format "~a~a;\n" indent (Basic b))]

      [(void) ()]
      ;; Set!'s might have non-basic RHS:
      [(set! ,v ,[(Statement "" tokargs) -> rhs])
       (list (format "~a~a = ~a" indent v rhs))]
      [(,prim ,[Basic -> args] ...)
       (guard (token-machine-primitive? prim))
       (define (e1) (Basic (car args)))
       (define (e2) (Basic (cadr args)))
       (define (e3) (Basic (caddr args)))
       (define (err) (format "~a // Primitive ~a not available!\n" indent prim))
       (case prim
	 [(+ - * / < > <= >=)
	  (format "(~a ~a ~a)" (e1) prim (e2))]
	 [(my-id) "TOS_LOCAL_ADDRESS"]
	 ;; Not implemented yet:
	 [(lightup loc locdiff rgb drawmark)
	  (err)]
	 [(dbg)
	  (apply format "~adbg(DBG_USR1, ~s~a)"
		 indent (car args)
		 (prepend-all ", " (map Basic (cdr args))))]
	 )]
      
      [(if ,[Basic -> t] 
	   (begin ,[(Statement (++ "  " indent) tokargs) -> c*] ...) 
	   (begin ,[(Statement (++ "  " indent) tokargs) -> a*] ...))
       (apply ++
	      `(indent 
		"if (" ,t ") {\n"
		,@c* ;(map list (make-list (length c*) indent) c*)
		,indent "} else {\n"
		,@a* ; (map list (make-list (length a*) indent) a*)
		,indent "}\n"))]

					;    TMS.Ssense (Just (Id id)) -> [ indent ++ id ++ " = call ADC.getData();\n" ]
					;    TMS.Ssense Nothing        -> error "shouldn't have Ssense with no storage location"

      ;;    -- FIXME	   
      [(call (tok ,[symbol->string t] ,[Basic -> sub]) ,[Basic -> args])
       (list
	(++ indent "the_packet.type = " (tok_id t) ";\n")
	(apply ++ `(,indent "dbg(DBG_USR1, \"TM TestMachine: local call to: " ,t 
			    " with " ,(number->string (length args)) " args: "
			    ,@(make-list (length args) " %d") "\\n\""
			    ,@(prepend-all ", " args) ");\n"))
	(apply ++ `(,indent "call token_" (tokname t) "("
			    ,@(insert-between ", " args) ");\n")))]
					;-- FIXME FILL IN ARG DATA HERE...
					;--indent ++"/* Should fill in arg data here... */\n",
					;-- Not going through the FIFO right now...
					;--indent++"call TMComm_"++ tokname t ++".add_msg(the_packet);\n" 
					;--indent ++"/* It's sketchy that I use the_packet for this without copying it... */\n",

					;--    Ssocreturn b -> []--[ indent ++ "call TMComm.socreturn("++ process_basic b ++");\n" ]
					;--    Ssocfinished -> []--[ indent ++ "call TMComm.socfinished();\n"]
      

      [(return ,[Basic -> b])
       (list (++ indent "return " b ";\n"))]
      [(leds ,what ,which)
       (let ([what (case what
		     [(on) "On"]
		     [(off) "Off"]
		     [(toggle) "Toggle"])])
	 (format "~acall Leds.~a~a();\n" indent which what))]

      [,other (error 'emit-nesc:Statement  "couldn't handle: ~s" other)]

      )))

					; process_block :: String -> [String] -> TMS.Block -> ([FunctionCode],[StatementCode])
					; process_block indent tokargs (TMS.Block locals stmts) = 
					;     let body = concat $ map (process_stmt indent tokargs) stmts
					;         defs = concat $ map (process_localdef indent) locals
					;     in ([], 
					; 	defs ++ body)

					; process_localdef :: String -> Id -> [String]
					; process_localdef indent (Id id) = [indent ++"uint16_t "++ id ++";\n"]

					; build_fun formals body = 
					;     " foo " 

					; litstr s = concat $ map (\c -> Data.Char.showLitChar c "") s

					; extra_args ls = concat $ map (", "++) ls

;; -------------------------------------------------------------------------------
;;  HERES THE MODULE IMPLEMENTATION GENERATOR.

;-- Well, what do we need to do here?  
;-- Structure as tasks, assign active messages...
;-- flatten and generate code for handlers.

(define ConstBind
  (match-lambda ((,name ,c))
    (format "  uint16_t ~a ~a;\n" name (Basic c))))

;-- Returns a string for the handler (which goes in the switch statement)
;-- And another string ....
;process_handler :: TMS.TokHandler -> ([FunctionCode],StatementCode)
;process_handler (t, args, blk) = 

(define (Handler tb)
  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
					;(define argnames (map symbol->string args))
    ;; Body is assumed to be let-flattened:
    (match body
      [(let* (,[ConstBind -> localbinds] ...) 
	 (begin ,[(Statement "        " args)-> stmts] ...))       
       ;; Return code for function defs, as well as code for the switch statement.
       (vector (apply ++ 
	      `("  command uint16_t token_" ,(tokname tok) "(" 
		,@(insert-between ", " (prepend-all "uint16_t " (map symbol->string args)))
		") { \n"
		,@(mapi (lambda (i name) (format "    // Argument ~a is ~a \n" i name)) args)
		,@stmts

		"    return 0; // This is a kind of lame default return value.\n"
		"  }\n"
		;,@funs ;; [2005.11.10] No functions generated from processing statements currently
		))
	       (apply ++
		`("      case " ,(tok_id tok) ": \n"		    
                  ;-- DEBUG CODE
		  "        dbg(DBG_USR1, \"TM TestMachine: tok fired: type %d \\n\", tok);\n"++ 
		  ;--- DEBUG CODE
		  "        call token_" (tokname t) "("
		  ,@(insert-between 
		  ", " (mapi (lambda (i _) (format "args[~a]" i)) args))
		  ");\n"
		  "        break;\n")))])))


;; process_handlers :: [TMS.TokHandler] -> String
(define (Handlers tbs)
  (let-match ([(#(,funs ,bods) ...) (map Handler tbs)])
    (++ funs ;(apply ++ funs) 
"
command void apply_token(uint16_t tok, uint16_t* args) {
    switch (tok) {
    "(apply ++ bods)"
    default:
      dbg(DBG_USR1, \"TM TestMachine: apply_token, UNMATCHED TOK: %d\\n\", tok);
    }
  }

  // This is the main token-processing function:
  // Like receiveMsg, it must return a TOS_MsgPtr to replace the one it consumes.
  command TOS_MsgPtr TMModule.process_token(TOS_MsgPtr msg) { 
    TM_Payload* payload = (TM_Payload*)msg->data;
    uint16_t* args = (uint16_t*)(payload->args);
    call apply_token(msg->type, args);
    return msg;
  }
\n")))


;;process_startup :: [Token] -> String
(define (process_startup _) "")

;; -------------------------------------------------------------------------------

;; build_module_header :: [TMS.TokHandler] -> String
(define (build_module_header toks)
  (let ([toknames (map symbol->string (map handler->tokname toks))]
	[arglsts  (map handler->formals toks)])
    (++ "
module " modname "M 
{
  provides interface StdControl as Control; 
  provides interface TMModule; 
  provides command void start_socpgm();
  // Helper functions only: 
  provides command void apply_token(uint16_t tok, uint16_t* args);
  uses interface Leds;"
  (apply ++ 
	 (map (lambda (name args)
		(++ "  provides command uint16_t token_" name "("
		    (apply ++ (insert-between ", " (prepend-all "uint16_t " (map symbol->string args))))
		    ");\n")))
	 toknames arglsts)
  "\n"
  (apply ++ 	 
         (map (lambda (name)
		(++ "  uses interface TMComm as TMComm_" name "; \n"))
	   toknames))
  ;; TODO: FIXME REMOVE THIS:
  "  uses interface TMComm as TMComm_return_channel; \n"++
  "}\n\n")))



;;build_implementation_header :: [TMS.TokHandler] -> [Token] -> String
(define (build_implementation_header toks startup)
    "\n")
;build_implementation_footer :: [TMS.TokHandler] -> [Token] -> String
(define (build_implementation_footer tokhands startup)
  (++ "  
  command result_t Control.init() {
    the_payload_args = (uint16_t*)(the_payload->args);
    the_retpayload_args = (uint16_t*)(the_retpayload->return_vals);
    return SUCCESS;
  }

  command result_t Control.start() {
    // This is where we need to start the socpgm if we're SOC.
    if ( TOS_LOCAL_ADDRESS == BASE_STATION ) {
      call start_socpgm();
    }
    
    // Uh, was this supposed to call node-start??
    // Clear the arguments here... [- ??? -] 
    // call TMComm_" (tokname startup) ".add_msg(the_packet);
    return SUCCESS;
  }

  command result_t Control.stop() {
    return SUCCESS;
  }
"))


;; build_socfun :: [TMS.ConstBind] -> TMS.Block -> String
(define (build_socfun consts block) ;(TMS.Block locals stmts) =
  (let ((indent "    "))
    (++ "  task void socpgm() {\n"
     ; --"    dbg(DBG_USR1, \"TM TestMachine: starting soc program...\\n\");\n"++
	(apply ++ (prepend-all indent (map ConstBind consts)))
	
					;-- FIXME TODO INCLUDE FUNS HERE!!!
					;-- Generate code for all the statements in 
	(apply ++ (apply append (map (Statement indent ()) block ;stmts
                                     )))
	"  }\n\n"
	"  command void start_socpgm() {\n"
	"    post socpgm();\n"
	"  }\n\n")))

(define (build_module prog) ; (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
  (match prog
    [(,lang '(program (bindings ,cb* ...) (nodepgm (tokens ,tb* ...))))
      ;-- First spit out just a little header:
     (++ "
// Automatically generated module for " (number->string (length tb*)) " token handlers:
" (apply ++ (map (lambda (tb) (format "//   ~a\n" (handler->tokname tb)))) tb*)	"
includes TestMachine;
includes TokenMachineRuntime;

" (build_module_header tb*) "
implementation {
  TOS_Msg the_packet;
  // Both of these are pointers into the structure of 'the_packet'
  TM_Payload* the_payload = (TM_Payload*)(the_packet.data);
  uint16_t* the_payload_args;
  TM_ReturnPayload* the_retpayload = (TM_ReturnPayload*)(the_packet.data);
  uint16_t* the_retpayload_args;
    
  // The constant bindings:
" (apply ++ (map ConstBind cb*))
  (build_implementation_header tb* '(node-start))
  ;(map ConstBind socconsts)
"
  // Token handlers and their helper functions:
" (Handlers tb*)
  (build_socfun socconsts socpgm)
  ;  -- Put the StdControl stuff in the footer:
  (handwritten_helpers (map handler->tokname tb*))
  (build_implementation_footer tb* 'node-start) "
}
")]))

;-- [2005.11.09] Don't have any of these now that I am removing all gradient code.
(define (handwritten_helpers toks) "\n")
;    let mkcase s t = "      case "++tok_id t++": \n"++
;		     "        return call TMComm_"++ tokname t ++"."++ s ++";\n"++ 
;		     "      break;\n"
;    in ("\n")


;; -------------------------------------------------------------------------------
;;  HERES THE CONFIGURATION GENERATOR. 

(define (build-configuration toknames)
  (apply string-append `("
// Automatically generated configuration file
includes TestMachine;
includes TokenMachineRuntime;
 configuration " modname "
 {
 }
 implementation
 {
 components " modname "M, Main, TimerC, LedsC, BasicTMComm, GenericComm as Comm;
   Main.StdControl -> TestMachineM.Control;
   Main.StdControl -> Comm;
   Main.StdControl -> TimerC;
   BasicTMComm.TMModule -> TestMachineM.TMModule;
   TestMachineM.Leds -> LedsC;
"
 ,@(apply append 
	  (map (lambda (t)
		 (list "  TestMachineM.TMComm_" (tokname t) " -> BasicTMComm.TMComm[" (tok_id t) "];\n"))
	    toknames))
 "TestMachineM.TMComm_return_channel -> BasicTMComm.TMComm[AM_RETURNMSG];
 }\n")))



;; -------------------------------------------------------------------------------

(define (build_header_file prog) ; (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
  (match prog
    [(,lang '(program (bindings ,cb* ...) (nodepgm (tokens ,tb* ...))))
     (++ "
#define BASE_STATION 0
#define NUM_TOKS " (number->string (length nodetoks)) "
enum {
" 
(apply ++ (mapi (lambda (i t)
		  (format "~a = ~a,\n" (tok_id t) i))
		(append (list socret_target );socfinished_target 
			(map handler->tokname tb*))))
"};
")]))

#|
#|
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

	  prog <- catch (do str <- readFile filename
			    putStr ("Read tokmac from file: "++filename++"\n")
			    return (read str :: TMPgm))
		  (\e -> if IO.isDoesNotExistError e 
     		         then (do putStr "File not found.  Compiling default tokmac.\n"
			          --return (b :: TMPgm)
			          error "No file."
			      )
		         else ioError e)

	  putStr "Expanding token machine... \n" 
	  let prog2 = prog --expand_tm prog
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
|#



|#

(define emit-nesc
  (let ()
    
    (lambda (_) (void))
))



(define these-tests
  `(
    [(Basic ''1) "1"]
    [(Basic 'v) "v"]
    [(tokname 'v-1) "v1"]
    [(tokname '(tok v-1 33333)) "v1"]
    [(tokname "foo") "foo"]
    [(prepend-all "a" '("b" "c" "d")) ("ab" "ac" "ad")]
    [(basic? '(+ x y)) #f]
    [(let ((f (token-numberer))) (apply-ordered list (f 'foo) (f 'bar) (f 'foo) (f 'foo) (f 'bar) (f 'baz)))
     (0 1 0 0 1 2)]

    [((Statement "" ()) '(+ '1 x))
     ??]

    ))

(define test-this (default-unit-tester "Pass32: Emit NesC code." these-tests))
(define test32 test-this)





