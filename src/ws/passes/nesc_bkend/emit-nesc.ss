;; Pass Emit NesC [2005.11.09]

;; Generate NesC code.  Porting this from the haskell code.

;; Also see tossim.ss.  Run (test-nesc) for an example.


(module emit-nesc mzscheme
  (require "../../../plt/common.ss"
	   "../../../plt/hashtab.ss"
	               "../../compiler_components/c_generator.ss"
	   (all-except "../../compiler_components/tml_generic_traverse.ss" test-this these-tests))
  (provide emit-nesc test-this test-emit-nesc)
  (chezimports)

; --------------------------------------------------------------------------------

;; HACK: this is non-null for a reason
(define-testing these-tests '([1 1]))

(define emit-nesc-modname "TestMachine")

(define emit-nesc
  (let ()

(define (prepend-all x ls)
  (if (null? ls) ()
      (cons (** x (car ls)) (prepend-all x (cdr ls)))))

;; This should filter out illegal characters!!
(define (tokname x)
  (DEBUGASSERT (or (symbol? x) (string? x) (pair? x)))
    (match x
      [(tok ,t ,_) (mangle-name (symbol->string t))]
      [,s (guard (string? s)) (mangle-name s)]
      [,v (symbol? v) (mangle-name (symbol->string v))]))


(define (prog->tokens prog)
  (match prog
    [(,lang '(program (bindings ,cb* ...) (nodepgm (tokens ,tb* ...))))
     tb*]
    [,other (error 'prog->tokens "Bad program: ~s" other)]))

(define (upcase s) (list->string (map char-upcase (string->list s))))

;; This takes a token to a string identifying its ActiveMessage number:
(define (tok_id t)  (** "AM_" (upcase (tokname t))))

(define (dequote x) 
  (match x [(quote ,v) v] [,else (error 'dequote "not quoted! ~s" x)]))


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
(define (Basic x)
    (match x
      [,v (guard (symbol? v)) (symbol->string v)]
      [,n (guard (number? n)) (number->string n)]
      [(quote ,c) (guard (or (symbol? c) (simple-constant? c)))
       (if (boolean? c)
	   (if c "1" "0")
	   (format "~a" c))]
      ;; This is occuring in operand position, for now we allow only 256
      ;; tokens and 256 subids, this fitting the whole thing in a uint16_t
      [(tok ,t ,[id]) (format "(~a + ~a)" (* 256 (token->number t)) id)]
      ))

(define basic?
  (lambda (x)
    (match x
      [,v (guard (symbol? v)) #t]
      [(quote ,c) (guard (or (simple-constant? c) (symbol? c))) #t]
      [(tok ,t ,id) #t]
      [,else #f])))


;; This returns both:
;;   *) bunch of strings (functions) to be seperately
;;     stuck in the top-level of the module implementation, 
;;   *) a bunch of strings (statements) which must be placed in the 
;;      current basic block above the current expression (local bindings).
;;   *) a string (expression) that represents the appropriate value

;; [2005.11.13] Added a continuation argument.  For now I use this for
;; pushing variable setting inside IF branches.  Could use it for
;; other things in the future.
;; NOTE: K is applied before indentation is added.
;; NOTE: K is only applied if it's a VALUE.  Effect only forms just don't apply K.

;; FIXME: TODO: This will be much more sane if I have a normalize-context pass first.

;;  The 2nd argument is the "arg environment" for bound variables.
;process_stmt :: String -> [String] -> TMS.Stmt -> [StatementCode]
(define (Expr indent k tokargs)
  (DEBUGASSERT (procedure? k))
  (lambda (e)
    (match e
      [,b (guard (basic? b)) 
	  (list (format "~a~a;\n" indent (k (Basic b))))]

      [(void) ()]

      ;; Set!'s might have non-basic RHS:
      [(set! ,v ,rhs)
       ;; Here we add to the continuation:
       ((Expr indent (lambda (x) (format "~a = ~a" v (k x))) tokargs)
	      rhs)]
      
      [(if ,[Basic -> t] 
	   (begin ,[(Expr (** "  " indent) k tokargs) -> c*] ...) 
	   (begin ,[(Expr (** "  " indent) k tokargs) -> a*] ...))       
       `(,(** indent "if (" t ") {\n")	 
	 ,@(apply append c*) ;(map list (make-list (length c*) indent) c*)
	 ,(** indent "} else {\n")
	 ,@(apply append a*) ; (map list (make-list (length a*) indent) a*)
	 ,(** indent "}\n"))]

					;    TMS.Ssense (Just (Id id)) -> [ indent ++ id ++ " = call ADC.getData();\n" ]
					;    TMS.Ssense Nothing        -> error "shouldn't have Ssense with no storage location"


      [(begin ,x* ... ,y)
;       (let ((x* ((Expr indent (lambda (x) (format "~a~a;\n" indent x))) x*))
;	     (y  ((Expr indent (lambda (x) (format "~a~a;\n" indent (k x)))))))
       ;; Call the non tails with a simple continuation:
       (let ((x* (apply append (map (Expr indent (lambda (x) (format "~a;\n" x)) tokargs) x*)))
	     (y*  ((Expr indent (lambda (x) (format "~a;\n" (k x))) tokargs) y)))
	 (append x* y*))]

      ;;    -- FIXME	   
      [(call (tok ,[symbol->string -> t] ,[Basic -> sub]) ,[Basic -> args] ...)
       (disp "CALL" t sub args)
       
       (list
	(** indent "the_packet.type = " (tok_id t) ";\n")
	(apply ** `(,indent "dbg(DBG_USR1, \"TM TestMachine: local call to: " ,t 
			    " with " ,(number->string (length args)) " args: "
			    ,@(make-list (length args) " %d") "\\n\""
			    ,@(prepend-all ", " args) ");\n"))
	(** indent 
	    (k (apply ** `("call token_" ,(tokname t) "(" ,@(insert-between ", " args) ")")))
	    ";\n")
	)]
					;-- FIXME FILL IN ARG DATA HERE...
					;--indent **"/* Should fill in arg data here... */\n",
					;-- Not going through the FIFO right now...
					;--indent**"call TMComm_"** tokname t **".add_msg(the_packet);\n" 
					;--indent **"/* It's sketchy that I use the_packet for this without copying it... */\n",

					;--    Ssocreturn b -> []--[ indent ** "call TMComm.socreturn("** process_basic b **");\n" ]
					;--    Ssocfinished -> []--[ indent ** "call TMComm.socfinished();\n"]
      
      [(,prim ,args ...)
       (guard (token-machine-primitive? prim))
       (define (e1) (Basic (car args)))
       (define (e2) (Basic (cadr args)))
       (define (e3) (Basic (caddr args)))
       (define (err) 
	 (warning 'emit-nesc "Primitive \"~a\" not available!!!!!\n" prim)
	 (format "~a // Primitive \"~a\" not available!!!!!\n" indent prim))
       (let ((result (case prim
		       [(+ - * / < > <= >=)
			(k (format "(~a ~a ~a);\n" (e1) prim (e2)))]
		       [(=) (k (format "(~a == ~a);\n" (e1) (e2)))]
		       [(my-id) (k "TOS_LOCAL_ADDRESS;\n")]
		       ;; Not implemented yet:
		       [(lightup loc locdiff rgb drawmark)
			(err)]

		       [(dbg printf)
			(format "dbg(DBG_USR1, ~s~a);\n"
				(if (eq? prim 'dbg)
				    (** "TMDBG: " (dequote (car args)))
				    (** "TMPRNT: " (dequote (car args))))
				(apply ** (prepend-all ", " (map Basic (cdr args)))))]
		       [else (err)])))
	 (list (** indent result)))]


      [(return ,[Basic -> b])
       (list (** indent "return " b ";\n"))]
      [(leds ,what ,which)
       (let ([what (case what
		     [(on) "On"]
		     [(off) "Off"]
		     [(toggle) "Toggle"])])
	 (format "~acall Leds.~a~a();\n" indent which what))]

      [,other (error 'emit-nesc:Expr  "couldn't handle: ~s" other)]

      )))

					; process_block :: String -> [String] -> TMS.Block -> ([FunctionCode],[ExprCode])
					; process_block indent tokargs (TMS.Block locals stmts) = 
					;     let body = concat $ map (process_stmt indent tokargs) stmts
					;         defs = concat $ map (process_localdef indent) locals
					;     in ([], 
					; 	defs ** body)

					; process_localdef :: String -> Id -> [String]
					; process_localdef indent (Id id) = [indent **"uint16_t "** id **";\n"]

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
    (format "uint16_t ~a = ~a;\n" name (Basic c))))


;; A "block" is just the let* expression.
(define (Block indent args b)
  (match b
    [(let* (,[ConstBind -> localbinds] ...) 
       (begin ,[(Expr indent id args) -> stmts] ...))
     (let ((result (apply append (prepend-all indent localbinds)
			  stmts)))
       (DEBUGASSERT (list? result))
       result)]
    [,other (error 'Block "expected let* block, received: ~s" other)]))

;-- Returns a string for the handler (which goes in the switch statement)
;-- And another string ....
;process_handler :: TMS.TokHandler -> ([FunctionCode],StatementCode)
;process_handler (t, args, blk) = 

(define (Handler tb)
  (mvlet ([(tok subid args stored bindings body) (destructure-tokbind tb)])
       ;; Return code for function defs, as well as code for the switch statement.
       (vector (apply ** 
	      `("  command uint16_t token_" ,(tokname tok) "(" 
		,@(insert-between ", " (prepend-all "uint16_t " (map symbol->string args)))
		") { \n"
		,@(mapi (lambda (i name) (format "    // Argument ~a is ~a \n" i name)) args)

		;; Body is assumed to be let-flattened:
		,@(Block "   " args body)

		"    return 0; // This is a kind of lame default return value.\n"
		"  }\n"
		;,@funs ;; [2005.11.10] No functions generated from processing statements currently
		))
	       (apply **
		`("     case " ,(tok_id tok) ": \n"		    
                  ;-- DEBUG CODE
		  "        dbg(DBG_USR1, \"TM TestMachine: tok fired: type %d \\n\", tok);\n"
		  ;--- DEBUG CODE
		  "        call token_" ,(tokname tok) "("
		  ,@(insert-between 
		  ", " (mapi (lambda (i _) (format "args[~a]" i)) args))
		  ");\n"
		  "        break;\n")))))


;; process_handlers :: [TMS.TokHandler] -> String
(define (Handlers tbs)
  (let-match ([(#(,funs ,bods) ...) (map Handler tbs)])
    (** (apply ** funs)
"
command void apply_token(uint16_t tok, uint16_t* args) {
    switch (tok) {
" (apply ** bods) "
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
    (** "
module " emit-nesc-modname "M 
{
  provides interface StdControl as Control; 
  provides interface TMModule; 
  provides command void start_socpgm();
  // Helper functions only: 
  provides command void apply_token(uint16_t tok, uint16_t* args);
  uses interface Leds;\n"
  (apply ** 
	 (map (lambda (name args)
		(** "  provides command uint16_t token_" (tokname name) "("
		    (apply ** (insert-between ", " (prepend-all "uint16_t " (map symbol->string args))))
		    ");\n"))
	   toknames arglsts))
  "
  uses interface TMComm;
  }
\n")))



;;build_implementation_header :: [TMS.TokHandler] -> [Token] -> String
(define (build_implementation_header toks startup)
    "\n")
;build_implementation_footer :: [TMS.TokHandler] -> [Token] -> String
(define (build_implementation_footer tokhands startup)
  (** "  
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
    // call TMComm.add_msg(the_packet);
    return SUCCESS;
  }

  command result_t Control.stop() {
    return SUCCESS;
  }
"))


;; build_socfun :: [TMS.ConstBind] -> TMS.Block -> String
(define (build_socfun consts block) ;(TMS.Block locals stmts) =
  (let ((indent "    "))
    (** "  task void socpgm() {\n"
     ; --"    dbg(DBG_USR1, \"TM TestMachine: starting soc program...\\n\");\n"++
	(apply ** (prepend-all indent (map ConstBind consts)))
					;-- FIXME TODO INCLUDE FUNS HERE!!!
					;-- Generate code for all the statements in 

;	(apply ** (apply append (map (Expr indent id ()) block ;stmts
;                                     )))
	(apply ** (Block indent () block))

	"  }\n\n"
	"  command void start_socpgm() {\n"
	"    post socpgm();\n"
	"  }\n\n")))

(define (build_module prog) ; (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
  (match prog
    [(,lang '(program (bindings ,cb* ...) (nodepgm (tokens ,tb* ...))))
      ;-- First spit out just a little header:
     (** "
// " (date-and-time) "
// Automatically generated module for " (number->string (length tb*)) " token handlers:
" (apply ** (map (lambda (tb) (format "//   ~a\n" (handler->tokname tb))) tb*))	"
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
" (apply ** "  " (map ConstBind cb*))
  (build_implementation_header tb* 'node-start)
  ;(map ConstBind socconsts)
"
  // Token handlers and their helper functions:
" (Handlers tb*)
(let ((entry (assq 'SOC-start tb*)))
  (if entry 
      (build_socfun () ;socconsts
;		    (match (handler->body entry) ;socpgm
;		      [(let* () (begin ,xps ...)) xps]
;		      [,other (error 'emit-nesc:build_module
;				     "Invalid 
		    (handler->body entry)
		    )
      (error 'emit-nesc:build_module "no binding for SOC-start")))
  ;  -- Put the StdControl stuff in the footer:
  (handwritten_helpers (map handler->tokname tb*))
  (build_implementation_footer tb* 'node-start) "
}
")]))

;-- [2005.11.09] Don't have any of these now that I am removing all gradient code.
(define (handwritten_helpers toks) "\n")
;    let mkcase s t = "      case "++tok_id t++": \n"++
;		     "        return call TMComm_"** tokname t **"."** s **";\n"** 
;		     "      break;\n"
;    in ("\n")


;; -------------------------------------------------------------------------------
;;  HERES THE CONFIGURATION GENERATOR. 

(define (build_configuration prog)
  (let ((toknames (map handler->tokname (prog->tokens prog))))
    (apply ** `("
// " ,(date-and-time) "
// Automatically generated configuration file
includes TestMachine;
includes TokenMachineRuntime;
 configuration " ,emit-nesc-modname "
 {
 }
 implementation
 {
 components " ,emit-nesc-modname "M, Main, TimerC, LedsC, BasicTMComm, GenericComm as Comm;
   Main.StdControl -> TestMachineM.Control;
   Main.StdControl -> Comm;
   Main.StdControl -> TimerC;
   BasicTMComm.TMModule -> TestMachineM.TMModule;
   TestMachineM.Leds -> LedsC;
   TestMachineM.TMComm -> BasicTMComm.TMComm[0];
 }\n"))))

;; -------------------------------------------------------------------------------

(define (build_header_file prog) ; (TMS.Pgm consts socconsts socpgm nodetoks startup) = 
  (match prog
    [(,lang '(program (bindings ,cb* ...) (nodepgm (tokens ,tb* ...))))
     (** "
// " (date-and-time) "
// Automatically generated header file supporting Token Machine code.
#define BASE_STATION 0
#define NUM_TOKS " (number->string (length tb*)) "
enum {
" 
(apply ** (prepend-all 
	   "  "
	   (mapi (lambda (i t)
		  (format "~a = ~a,\n" (tok_id t) i))
		(append (list socret_target );socfinished_target 
			(map handler->tokname tb*)))))
"};
")]))

;; ----------------------------------------------------------------------

(define (assemble prog)
  (vector (build_module prog)
	  (build_configuration prog)
	  (build_header_file prog)
	  ))

; =======================================================================
;; Testing


(define hand1 '(tok1 sid () (stored) (let* () (begin (printf '"test!\n")))))
(define hand2 '(tok2 sid2 () (stored (x '33)) (let* ((y '44)) (begin (+ x y)))))

(define prog `(lang '(program (bindings) 
		       (nodepgm (tokens ,hand1 ,hand2 
					(SOC-start _ () (stored) (let* () (begin (void)))))))))

(unless (null? these-tests)
 (set! these-tests
      (append `(
    [(,Basic ''1) "1"]
    [(,Basic 'v) "v"]
    [(,tokname 'v-1) "v1"]
    [(,tokname '(tok v-1 33333)) "v1"]
    [(,tokname "foo") "foo"]
    [(,prepend-all "a" '("b" "c" "d")) ("ab" "ac" "ad")]
    [(,basic? '(+ x y)) #f]
    [(let ((f (,token-numberer))) (apply-ordered list (f 'foo) (f 'bar) (f 'foo) (f 'foo) (f 'bar) (f 'baz)))
     (0 1 0 0 1 2)]

    [((,Expr "" (lambda (x) x) ()) '(+ '1 x))
     ("(1 + x);\n")]
    [((,Expr "" (lambda (x) x) ()) '(set! v (if '#t (begin '3) (begin '4))))
     ("if (1) {\n" "  v = 3;\n" "} else {\n" "  v = 4;\n" "}\n")]
    [(rac ((,Expr "" (lambda (x) x) ()) '(call (tok f 0) '3)))
     "call token_f(3);\n"]

    [((,Expr "<>" (lambda (x) x) ()) '(printf '"test!\n"))
     ("<>dbg(DBG_USR1, \"TMPRNT: test!\\n\");\n")]

    [(,ConstBind '(v '3)) "uint16_t v = 3;\n"]

    [(,Handler ',hand1)
     ,(lambda (x) (and (vector? x) (= (vector-length x) 2)))]
    [(,Handlers ',(list hand1 hand2))
     unspecified]

    [(emit-nesc ',prog) unspecified]

    ) 
	      these-tests)))

;; --------------------------------------------------
;; Main body of emit-nesc:
(lambda (prog) 
  `(emit-nesc-language
    ,(assemble prog)))
))


(define-testing test-this (default-unit-tester "Pass32: Emit NesC code." these-tests))
(define test32 test-this)
(define test-emit-nesc test-this)

) ; End module
